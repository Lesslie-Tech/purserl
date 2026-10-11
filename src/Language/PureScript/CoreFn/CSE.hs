-- | This module performs limited common subexpression elimination
module Language.PureScript.CoreFn.CSE (optimizeCommonSubexpressions) where

import Protolude

import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Supply (Supply)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Semigroup (Min(..))

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.Constants.Libs qualified as C
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..), pattern App3)
import Language.PureScript.CoreFn.Meta (Meta(IsSyntheticApp))
import Language.PureScript.CoreFn.Traversals (everywhereOnValues)
import Language.PureScript.Environment (dictTypeName)
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, ProperName(..), Qualified(..), QualifiedBy(..), freshIdent, runIdent, toMaybeModuleName)
import Language.PureScript.PSString (decodeString)

-- |
-- Float synthetic Apps (right now, the only Apps marked as synthetic are type
-- class dictionaries being fed to functions with constraints, superclass
-- accessors, and instances of IsSymbol) to a new or existing Let as close to
-- the top level as possible.
--
-- Scopes are numbered by nesting depth (Abs, Let/top-level bindings and case
-- alternatives each open one). An expression is floated to the deepest scope
-- that binds one of its free variables, so every identical floated expression
-- in a scope shares a single binding. Bindings that end up used only once (and
-- not under a lambda) are put back where they came from.
optimizeCommonSubexpressions :: ModuleName -> [Bind Ann] -> Supply [Bind Ann]
optimizeCommonSubexpressions mn decls = Strict.evalStateT run emptyState
  where
  env0 = Env { currentModule = mn, depth = 0, topLevelDepth = 0, bound = M.empty }
  run = do
    ((decls', ()), info) <- bindsThen env0 True (\_ -> pure ((), mempty)) decls
    (floated, _) <- closeScope 0 info
    reinlined <- gets reinline
    pure $ replaceLocals reinlined (floated ++ decls')

-- |
-- Whether a floated identifier is used more than once (True) or once (False).
-- A single use inside an Abs counts as many, as it is unknown how often the Abs
-- will run.
--
type Uses = M.Map Ident Bool

-- |
-- What is known about an expression once it has been processed.
--
data Info = Info
  { scopesUsed :: IS.IntSet
    -- ^ the scope numbers of the variables used in the expression
  , noFloatWithin :: Maybe (Min Int)
    -- ^ the scope in which a recursively bound identifier is used, within
    -- which the expression must not be floated
  , uses :: Uses
    -- ^ the floated identifiers used in the expression
  }

instance Semigroup Info where
  Info s1 n1 u1 <> Info s2 n2 u2 = Info (s1 <> s2) (n1 <> n2) (M.unionWith (\_ _ -> True) u1 u2)

instance Monoid Info where
  mempty = Info mempty Nothing mempty

-- |
-- Everything within an Abs may run several times.
--
enterAbs :: Info -> Info
enterAbs i = i { uses = map (const True) (uses i) }

-- |
-- Forget the scope being left: its variables can't influence outer expressions.
--
leaveScope :: Int -> Info -> Info
leaveScope d i = i
  { scopesUsed = IS.filter (< d) (scopesUsed i)
  , noFloatWithin = find (< Min d) (noFloatWithin i)
  }

data BindingType = NonRecursive | Recursive deriving Eq

data Env = Env
  { currentModule :: ModuleName
  , depth :: Int
    -- ^ number of enclosing binding scopes
  , topLevelDepth :: Int
    -- ^ number of enclosing scopes outside the first Abs; floated identifiers
    -- at or above it are qualified with the module name
  , bound :: M.Map Ident (Int, BindingType)
    -- ^ the depth at which identifiers are bound, and whether recursively
  }

data CSEState = CSEState
  { floatedAt :: IM.IntMap (M.Map (Expr ()) Ident)
    -- ^ the identifier given to each floated expression, by scope depth
  , pending :: IM.IntMap [(Ident, Uses, Expr Ann)]
    -- ^ floated bindings not yet placed, newest first, by scope depth, along
    -- with the floated identifiers each uses
  , reinline :: M.Map Ident (Expr Ann)
    -- ^ floated identifiers that did not need a binding after all
  }

emptyState :: CSEState
emptyState = CSEState IM.empty IM.empty M.empty

type CSE = Strict.StateT CSEState Supply

-- | A result together with what is known about the expression it came from.
type R a = CSE (a, Info)

-- |
-- Process a list of items in order, combining what is known about them.
--
traverseR :: (a -> R b) -> [a] -> R [b]
traverseR f xs = do
  rs <- traverse f xs
  pure (map fst rs, foldMap snd rs)

-- |
-- Process the contents of a new scope in which the given identifiers are bound.
--
inScope :: Env -> Bool -> [Ident] -> (Env -> R a) -> R a
inScope env isTopLevel idents body = do
  let d = depth env + 1
      env' = env
        { depth = d
        , topLevelDepth = if isTopLevel then d else topLevelDepth env
        , bound = foldl' (\m i -> M.insert i (d, NonRecursive) m) (bound env) idents
        }
  second (leaveScope d) <$> body env'

-- |
-- Place the floated bindings that belong to the scope at the given depth, which
-- is ending: those used more than once become binds, the rest are marked for
-- reinlining.
--
closeScope :: Int -> Info -> CSE ([Bind Ann], Info)
closeScope d info = do
  floated <- gets (IM.findWithDefault [] d . pending)
  modify $ \s -> s { floatedAt = IM.delete d (floatedAt s), pending = IM.delete d (pending s) }
  foldM place ([], info) floated
  where
  -- Newest first, as a binding's uses are only known once everything that
  -- uses it has been placed.
  place (binds, i) (ident, usesInBody, e) = do
    let i' = i { uses = M.unionWith (\_ _ -> True) (uses i) usesInBody }
    if fromJust (M.lookup ident (uses i))
      then pure (NonRec nullAnn ident e : binds, i')
      else do
        modify $ \s -> s { reinline = M.insert ident e (reinline s) }
        pure (binds, i')

-- |
-- Process an expression that ends a scope, wrapping it in a Let for the
-- bindings floated to that scope. If the expression is already a Let, the Binds
-- are added to it instead.
--
exprWithBinds :: Env -> Expr Ann -> R (Expr Ann)
exprWithBinds env e = do
  (e', i) <- expr env e
  (binds, i') <- closeScope (depth env) i
  pure (wrapInLet binds e', i')

wrapInLet :: [Bind Ann] -> Expr Ann -> Expr Ann
wrapInLet [] e = e
wrapInLet bs (Let a bs' e) = Let a (bs ++ bs') e
wrapInLet bs e = Let nullAnn bs e

nullAnn :: Ann
nullAnn = (nullSourceSpan, [], Nothing)

expr :: Env -> Expr Ann -> R (Expr Ann)
expr env e0 = do
  r@(e, _) <- case e0 of
    Abs a ident e1 -> do
      (e1', i) <- inScope env False [ident] (\env' -> exprWithBinds env' e1)
      pure (Abs a ident e1', enterAbs i)
    Var _ qname -> pure (e0, summarizeName env qname)
    Let a bs e1 -> do
      ((bs', e1'), i) <- bindsThen env False (\env' -> expr env' e1) bs
      pure (Let a bs' e1', i)
    Literal a (ArrayLiteral xs) -> do
      (xs', i) <- traverseR (expr env) xs
      pure (Literal a (ArrayLiteral xs'), i)
    Literal a (ObjectLiteral fs) -> do
      (vs, i) <- traverseR (expr env . snd) fs
      pure (Literal a (ObjectLiteral (zip (map fst fs) vs)), i)
    Accessor a prop e1 -> first (Accessor a prop) <$> expr env e1
    ObjectUpdate a obj copy fs -> do
      (obj', i1) <- expr env obj
      (vs, i2) <- traverseR (expr env . snd) fs
      pure (ObjectUpdate a obj' copy (zip (map fst fs) vs), i1 <> i2)
    App a f x -> do
      (f', i1) <- expr env f
      (x', i2) <- expr env x
      pure (App a f' x', i1 <> i2)
    Case a scrutinees alts -> do
      (scrutinees', i1) <- traverseR (expr env) scrutinees
      (alts', i2) <- traverseR (caseAlternative env) alts
      pure (Case a scrutinees' alts', i1 <> i2)
    Literal{} -> pure (e0, mempty)
    Constructor{} -> pure (e0, mempty)
  if shouldFloatExpr e then float env r else pure r

caseAlternative :: Env -> CaseAlternative Ann -> R (CaseAlternative Ann)
caseAlternative env (CaseAlternative bs x) =
  first (CaseAlternative bs) <$> inScope env False (identsFromBinders bs) (\env' ->
    case x of
      Left guarded -> first Left <$> traverseR (guardedExpr env') guarded
      Right e -> first Right <$> exprWithBinds env' e)
  where
  guardedExpr env' (g, e) = do
    (g', i1) <- exprWithBinds env' g
    (e', i2) <- exprWithBinds env' e
    pure ((g', e'), i1 <> i2)

-- |
-- Process a list of Binds followed by something that can refer to them.
--
bindsThen :: forall a. Env -> Bool -> (Env -> R a) -> [Bind Ann] -> R ([Bind Ann], a)
bindsThen env isTopLevel inner = \case
  [] -> first ([],) <$> inner env
  NonRec a ident e : rest -> do
    -- The bound expression is in the current scope; the remaining Binds and the
    -- inner thing are in a new scope in which the identifier is bound.
    (e', i1) <- expr env e
    (r, i2) <- inScope env isTopLevel [ident] (\env' -> restOf env' (NonRec a ident e') rest)
    pure (r, i1 <> i2)
  Rec es : rest ->
    -- The bound expressions are in a new scope in which all the identifiers are
    -- bound recursively; the remaining Binds and the inner thing are in the same
    -- scope with them bound non-recursively.
    inScope env isTopLevel [] $ \env' -> do
      let idents = map (snd . fst) es
          bindAs t en = en { bound = foldl' (\m i -> M.insert i (depth en, t) m) (bound en) idents }
      (es', i1) <- traverseR (\(k, e) -> first (k,) <$> expr (bindAs Recursive env') e) es
      (r, i2) <- restOf (bindAs NonRecursive env') (Rec es') rest
      pure (r, i1 <> i2)
  where
  restOf :: Env -> Bind Ann -> [Bind Ann] -> R ([Bind Ann], a)
  restOf env' hd rest = do
    ((restBinds, a), i) <- bindsThen env' isTopLevel inner rest
    (floated, i') <- closeScope (depth env') i
    pure ((hd : floated ++ restBinds, a), i')

-- |
-- Note which scope a variable comes from.
--
summarizeName :: Env -> Qualified Ident -> Info
summarizeName env (Qualified mn' ident) =
  Info (IS.singleton s) (guard (bt == Recursive) $> Min s) mempty
  where
  (s, bt) =
    fromMaybe (0, NonRecursive) $
      guard (all (== currentModule env) (toMaybeModuleName mn')) *> M.lookup ident (bound env)

-- |
-- Replace an expression with a reference to a binding for it, floated to the
-- deepest scope it uses.
--
float :: Env -> (Expr Ann, Info) -> R (Expr Ann)
float _ r@(_, Info { noFloatWithin = Just _ }) = pure r
float env (e, i) = do
  let d = if IS.null (scopesUsed i) then 0 else IS.findMax (scopesUsed i)
      key = void e
  existing <- gets (IM.lookup d . floatedAt >=> M.lookup key)
  ident <- case existing of
    Just ident -> pure ident
    Nothing -> do
      ident <- freshIdent (nameHint e)
      modify $ \s -> s
        { floatedAt = IM.insertWith M.union d (M.singleton key ident) (floatedAt s)
        , pending = IM.insertWith (++) d [(ident, uses i, e)] (pending s)
        }
      pure ident
  let qb = if d > topLevelDepth env then ByNullSourcePos else ByModuleName (currentModule env)
  pure (Var nullAnn (Qualified qb ident), i { uses = M.singleton ident False })

nameHint :: Expr a -> Text
nameHint = \case
  App _ v1 v2
    | Var _ n <- v1
    , fmap (ProperName . runIdent) n == fmap dictTypeName C.IsSymbol
    , Literal _ (ObjectLiteral [(_, Abs _ _ (Literal _ (StringLiteral str)))]) <- v2
    , Just decodedStr <- decodeString str
      -> decodedStr <> "IsSymbol"
    | otherwise
      -> nameHint v1
  Var _ (Qualified _ ident)
    | Ident name             <- ident -> name
    | GenIdent (Just name) _ <- ident -> name
  Accessor _ prop _
    | Just decodedProp <- decodeString prop -> decodedProp
  _ -> "ref"

-- This is the one place (I think?) that keeps this from being a general
-- common subexpression elimination pass.
shouldFloatExpr :: Expr Ann -> Bool
shouldFloatExpr = \case
  -- INVARIANT[drathier]: this forcefully skips CSE lifting for some type class functions, to allow them to be specialized in the Erl ast later on. See purescript/purerl/src/Language/PureScript/Erl/CodeGen/Optimizer/Inliner.hs:specialize.onErl
  App3 "Data.Semigroup" _ "Data.Semigroup" _ -> False
  App3 "Data.Semiring" _ "Data.Semiring" _ -> False
  App3 "Data.Ring" _ "Data.Ring" _ -> False
  App3 "Data.EuclideanRing" _ "Data.EuclideanRing" _ -> False
  App3 "Data.HeytingAlgebra" _ "Data.HeytingAlgebra" _ -> False
  App3 "Erl.Data.List.Types" _ "Erl.Data.List.Types" _ -> False
  App3 "Data.Ord" _ "Data.Ord" _ -> False
  App3 "Data.Eq" _ "Data.Eq" _ -> False
  App3 "Data.Int" _ "Data.Int" _ -> False
  App (_, _, Just IsSyntheticApp) e _ -> isSimple e
  _                                   -> False
  where
  isSimple :: Expr Ann -> Bool
  isSimple = \case
    Var{}          -> True
    Accessor _ _ e -> isSimple e
    _              -> False

-- |
-- Collect all the Idents put in scope by a list of Binders.
--
identsFromBinders :: [Binder a] -> [Ident]
identsFromBinders = foldMap identsFromBinder where
  identsFromBinder = \case
    LiteralBinder _ (ArrayLiteral xs)  -> identsFromBinders xs
    LiteralBinder _ (ObjectLiteral xs) -> identsFromBinders (map snd xs)
    VarBinder _ ident                  -> [ident]
    ConstructorBinder _ _ _ xs         -> identsFromBinders xs
    NamedBinder _ ident x              -> ident : identsFromBinder x
    LiteralBinder _ BooleanLiteral{}   -> []
    LiteralBinder _ CharLiteral{}      -> []
    LiteralBinder _ NumericLiteral{}   -> []
    LiteralBinder _ StringLiteral{}    -> []
    NullBinder{}                       -> []

-- |
-- Use a map to substitute local Vars in a list of Binds.
--
replaceLocals :: M.Map Ident (Expr Ann) -> [Bind Ann] -> [Bind Ann]
replaceLocals m = if M.null m then identity else map f' where
  (f', g', _) = everywhereOnValues identity f identity
  f e@(Var _ (Qualified _ ident)) = maybe e g' $ ident `M.lookup` m
  f e = e
