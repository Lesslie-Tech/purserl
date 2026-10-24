module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type, moduleName, traceM, State, evalState)
import Prelude (error)

import Control.Monad.Supply (Supply)
import Control.Monad.Supply.Class (fresh)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.CSE (optimizeCommonSubexpressions)
import Language.PureScript.CoreFn.Expr
    ( Bind(..),
      Expr(..),
      CaseAlternative(..),
      bindIdents,
      extractAnn )
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Traversals (everywhereOnValues, traverseCoreFn)
import Language.PureScript.Constants.Libs qualified as C
import System.IO.Unsafe
import Language.PureScript.Names (Ident(..), runIdent, ModuleName(..), QualifiedBy(..), pattern ByNullSourcePos, runModuleName, ProperName, ProperNameType(..), Qualified(..), freshIdent)
import Control.DeepSeq (force)
import Control.Monad.Trans.RWS.Strict (evalRWST, asks, local, RWST)
import Control.Monad.State.Strict
import Data.Text (Text, append, pack)
import Debug.Trace (traceM)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Data.Text qualified as T
import Debug.Trace qualified as Debug

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.Erl.CodeGen.AST (Atom(..))
import Language.PureScript.Erl.CodeGen.Common (runAtom)

import Language.PureScript.AST.SourcePos qualified as P
import Language.PureScript.PSString qualified as PS
import Language.PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import qualified Language.PureScript.Constants.Libs as C
import qualified Language.PureScript.Constants.Prim as C

-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module Ann -> Supply (Module Ann)
-- optimizeCoreFn m = pure m
optimizeCoreFn m = do
  -- Expand `composeFlippedF`/`>>>` chains into real `Abs`
  -- nodes first, upstream of everything else, so every later pass (arity
  -- counting, constructor-application detection, CSE, purerl's own
  -- Erlang-level optimizer, ...) treats a point-free composition exactly as
  -- if it had been hand-written as a lambda -- see expandComposeFlippedF.
  decls0 <- expandComposeFlippedF (moduleDecls m)
  let decls1 = translateBacktrace (moduleName m) (moduleForeign m) decls0
      decls2 = optimizeModuleDecls (moduleName m) (moduleForeign m) decls1
  -- `Control.Alt.alt`/`<|>` chains over a statically-known `Maybe`/`Either`/
  -- `Veither` dictionary -- see expandAltChains. Runs after
  -- optimizeModuleDecls so `x # Maybe.alt y` (piped through
  -- Data.Function.apply/applyFlipped) has already been reduced to a plain
  -- application by optimizeDataFunctionApply; still runs before CSE so a
  -- whole chain is seen at once.
  decls2a <- expandAltChains decls2
  -- Record operations over a literal `IsSymbol` dictionary -- see
  -- expandRecordOps. Must run before CSE, which would otherwise float the
  -- dictionary (and the partial application of it) out of the call site.
  decls2b <- expandRecordOps decls2a
  decls3 <- optimizeCommonSubexpressions (moduleName m) decls2b
  pure m {moduleDecls = decls3}

-- | `Control.Semigroupoid.composeFlippedF` (aliased as `>>>`)
-- is a plain, non-typeclass function: `composeFlippedF f g x = g (f x)`.
-- Used point-free (`f >>> g`, with no third argument in sight), it's just an
-- ordinary 2-argument partial application at the CoreFn level -- nothing
-- marks it as "this value is secretly a function" the way a literal
-- `\x -> ...` would be, so passes that key off an actual `Abs` (top-level
-- arity counting, constructor-application detection for e.g. `f >>> Just`,
-- etc.) miss it entirely, and it has to be hunted down again, ad hoc, in
-- every later pass that cares (as happened in purerl's own Erlang-level
-- optimizer). Rewriting it into a real `Abs` here fixes that at the root.
composeFlippedFIdent :: Qualified Ident
composeFlippedFIdent = Qualified (ByModuleName (ModuleName "Control.Semigroupoid")) (Ident "composeFlippedF")

matchComposeFlippedF2 :: Expr Ann -> Maybe (Expr Ann, Expr Ann)
matchComposeFlippedF2 (App _ (App _ (Var _ fn) f) g)
  | fn == composeFlippedFIdent = Just (f, g)
matchComposeFlippedF2 _ = Nothing

-- | >>> is infixr, so a nested chain only ever shows up on the right
-- (f1 >>> (f2 >>> f3)); explicit left-nesting via parens is also unfolded
-- here for generality. Flattening the whole chain up front (rather than
-- letting each >>> turn into its own nested lambda one at a time) means
-- `f1 >>> f2 >>> ... >>> fn` becomes a single `\x -> fn (... (f1 x) ...)`,
-- not n-1 lambdas each immediately invoking the next.
collectComposeChain :: Expr Ann -> [Expr Ann]
collectComposeChain e
  | Just (f, g) <- matchComposeFlippedF2 e = collectComposeChain f <> collectComposeChain g
  | otherwise = [e]

expandComposeFlippedF :: [Bind Ann] -> Supply [Bind Ann]
expandComposeFlippedF = traverse goBind
  where
    goExpr :: Expr Ann -> Supply (Expr Ann)
    goExpr e
      | Just (f, g) <- matchComposeFlippedF2 e = do
          -- Collect against the RAW (not-yet-recursed) subtree so a nested
          -- chain is caught here rather than after it's already become a
          -- lambda; each collected piece is then processed via `goExpr` in
          -- case it independently contains further composeFlippedF usage
          -- buried inside it (not at its own head position).
          let chain = collectComposeChain f <> collectComposeChain g
          chain' <- traverse goExpr chain
          x <- freshIdent "composeArg"
          let ann = extractAnn e
              xVar = Var ann (Qualified ByNullSourcePos x)
              body = foldl' (\acc fi -> App ann fi acc) xVar chain'
          pure $ Abs ann x body
      | otherwise = goExprOneLevel e

    goBinder :: Binder Ann -> Supply (Binder Ann)
    goBinder = goBinderOneLevel

    goCaseAlt :: CaseAlternative Ann -> Supply (CaseAlternative Ann)
    goCaseAlt = goCaseAltOneLevel

    (goBind, goExprOneLevel, goBinderOneLevel, goCaseAltOneLevel) =
      traverseCoreFn goBind goExpr goBinder goCaseAlt

-- | `x <|> y` (`Control.Alt.alt`) for a statically-known `Maybe`/`Either`
-- dictionary is a plain 2-clause short-circuit: "first operand that isn't
-- the type's designated failure value, else the last operand regardless".
-- That's exactly what Erlang's `maybe ... end` block expresses natively via
-- its `Pattern ?= Expr` short-circuit-on-non-match semantics: matching the
-- failure pattern means "keep trying the next line", anything else means
-- "stop here and return this value". Rewriting a whole `<|>` chain into one
-- `CodeGen.erlang "maybe ... end" {...}` call -- which purerl's own
-- Erlang-level Inliner.hs already turns into real Erlang source via its
-- pre-existing raw-source escape hatch -- produces a single flat `maybe`
-- block with one line per operand, instead of leaving each `<|>` as its own
-- typeclass-dictionary application for later passes to partially resolve.
altIdent :: Qualified Ident
altIdent = Qualified (ByModuleName (ModuleName "Control.Alt")) (Ident "alt")

-- | How to recognize "this operand failed" for a given `Alt` instance.
-- `SimpleFail` is a fixed Erlang pattern (`Nothing`, `Left _`). `Veither`'s
-- failure isn't a fixed pattern -- it's "any type other than `"_"`" -- so it
-- needs a `case` first, see veitherCheckLine.
data AltKind = SimpleFail Text | VeitherKind
  deriving (Eq)

-- | (dictionary identifier, how to recognize failure) for each `Alt`
-- instance this rewrite covers.
altDicts :: [(Qualified Ident, AltKind)]
altDicts =
  [ (Qualified (ByModuleName (ModuleName "Maybe")) (Ident "altMaybe"), SimpleFail "{nothing}"),
    (Qualified (ByModuleName (ModuleName "Either")) (Ident "altEither"), SimpleFail "{left, _}"),
    (Qualified (ByModuleName (ModuleName "Veither")) (Ident "altVeither"), VeitherKind)
  ]

codeGenErlangIdent :: Qualified Ident
codeGenErlangIdent = Qualified (ByModuleName (ModuleName "CodeGen")) (Ident "erlang")

-- | Each instance's own plain `alt` function (e.g. `Maybe.alt`), for direct
-- calls that bypass the `Control.Alt` typeclass dictionary entirely.
directAltIdents :: [(Qualified Ident, AltKind)]
directAltIdents =
  [ (Qualified (ByModuleName (ModuleName "Maybe")) (Ident "alt"), SimpleFail "{nothing}"),
    (Qualified (ByModuleName (ModuleName "Either")) (Ident "alt"), SimpleFail "{left, _}"),
    (Qualified (ByModuleName (ModuleName "Veither")) (Ident "alt"), VeitherKind)
  ]

matchAlt2 :: Expr Ann -> Maybe (AltKind, Expr Ann, Expr Ann)
matchAlt2 (App _ (App _ (App _ (Var _ fn) (Var _ dict)) a) b)
  | fn == altIdent, [kind] <- [k | (d, k) <- altDicts, d == dict] = Just (kind, a, b)
matchAlt2 (App _ (App _ (Var _ fn) a) b)
  | [kind] <- [k | (d, k) <- directAltIdents, d == fn] = Just (kind, a, b)
matchAlt2 _ = Nothing

-- | `<|>` is infixl, so a nested chain only ever shows up on the left
-- (a <|> b) <|> c; explicit right-nesting via parens is also unfolded here
-- for generality, mirroring collectComposeChain.
collectAltChain :: AltKind -> Expr Ann -> [Expr Ann]
collectAltChain kind e
  | Just (kind', a, b) <- matchAlt2 e, kind' == kind =
      collectAltChain kind a <> collectAltChain kind b
  | otherwise = [e]

-- | The local record module's label-indexed operations take an `IsSymbol`
-- dictionary only to learn the label, which at every call site with a literal
-- label is a constant. Rewrite them to native Erlang map syntax. A pipeline
-- of operations (`r # set @"a" x # modify @"b" f # insert @"c" y`) is fused
-- into a single `R#{a := X, b := F(maps:get(b, R)), c => Y}`, which the
-- efficiency guide recommends over one update per key. Calls through a
-- dictionary variable are left alone.
recordModule :: ModuleName
recordModule = ModuleName "R"

-- | How a pending update writes its label: `Assign` to a key the record has,
-- `Insert` of one it lacks, `Merge` of one it may or may not have.
-- | A name unique in the whole module. Erlang scopes a variable bound inside
-- `begin ... end` to the enclosing clause, and the later renaming only keeps
-- apart names that existed before this pass, so a repeated name would turn the
-- second binding into a match against the first.
freshRecIdent :: Text -> Supply Ident
freshRecIdent name = (\n -> Ident (name <> "@" <> T.pack (show n))) <$> fresh

data RecKind = Assign | Insert | Merge
  deriving (Eq)

data RecOp
  = OpSet PS.PSString (Expr Ann)
  | OpModify PS.PSString (Expr Ann)
  | OpInsert PS.PSString (Expr Ann)
  | OpDelete PS.PSString
  | OpRename PS.PSString PS.PSString
  | OpPut [(PS.PSString, Expr Ann)]
  | OpMergeOver (Expr Ann)
  | OpId

-- | The label of a dictionary built by the `IsSymbol` solver.
literalSymbolDict :: Expr Ann -> Maybe PS.PSString
literalSymbolDict (App _ (Var _ (Qualified _ (Ident "IsSymbol$Dict"))) (Literal _ (ObjectLiteral [(k, Abs _ _ (Literal _ (StringLiteral s)))])))
  | k == PS.mkString "reflectSymbol" = Just s
literalSymbolDict _ = Nothing

-- | (dictionaries taken, value arguments) for each rewritten function.
recordOpArity :: Text -> Maybe (Int, Int)
recordOpArity = \case
  "get" -> Just (1, 1)
  "set" -> Just (1, 2)
  "modify" -> Just (1, 2)
  "insert" -> Just (1, 2)
  "delete" -> Just (1, 1)
  "rename" -> Just (2, 1)
  "nub" -> Just (0, 1)
  "merge" -> Just (0, 2)
  "union" -> Just (0, 2)
  "disjointUnion" -> Just (0, 2)
  _ -> Nothing

-- | A call to a rewritten function, split into its function name, labels and
-- value arguments, when the dictionaries are literal and at most all the
-- value arguments are present.
matchRecordCall :: Expr Ann -> Maybe (Ann, Text, [PS.PSString], [Expr Ann], Int)
matchRecordCall e
  | (Var ann (Qualified (ByModuleName mn) (Ident fn)), args) <- unApp e [],
    mn == recordModule,
    Just (nDicts, nVals) <- recordOpArity fn,
    length args >= nDicts,
    length args - nDicts <= nVals,
    Just labels <- traverse literalSymbolDict (take nDicts args) =
      Just (ann, fn, labels, drop nDicts args, nVals)
  | otherwise = Nothing
  where
    unApp (App _ f a) acc = unApp f (a : acc)
    unApp f acc = (f, acc)

-- | The operation of a fully applied call, and the record it applies to.
matchRecordOp :: Expr Ann -> Maybe (RecOp, Expr Ann)
matchRecordOp e = do
  (_, fn, labels, vals, nVals) <- matchRecordCall e
  guard (length vals == nVals)
  case (fn, labels, vals) of
    ("set", [l], [v, r]) -> Just (OpSet l v, r)
    ("modify", [l], [f, r]) -> Just (OpModify l f, r)
    ("insert", [l], [v, r]) -> Just (OpInsert l v, r)
    ("delete", [l], [r]) -> Just (OpDelete l, r)
    ("rename", [from, to], [r]) -> Just (OpRename from to, r)
    ("nub", [], [r]) -> Just (OpId, r)
    (_, [], [Literal _ (ObjectLiteral fields), r])
      | fn `elem` ["merge", "union", "disjointUnion"] ->
          Just (OpPut fields, r)
    (_, [], [l, r])
      | fn `elem` ["merge", "union", "disjointUnion"] -> Just (OpMergeOver l, r)
    _ -> Nothing

-- | Pending state of a fused chain: the record being updated, labels to drop
-- from it, and the updates to apply to what is left. `stLet` holds a binding
-- for the record, introduced only once something reads from it.
data RecState = RecState
  { stBase :: Expr Ann,
    stLet :: Maybe (Ident, Expr Ann),
    stDels :: [PS.PSString],
    stUps :: [(PS.PSString, RecKind, Expr Ann)]
  }

expandRecordOps :: [Bind Ann] -> Supply [Bind Ann]
expandRecordOps = traverse goBind
  where
    goExpr :: Expr Ann -> Supply (Expr Ann)
    goExpr e
      | Just (_, "get", [l], [r], _) <- matchRecordCall e =
          Accessor (extractAnn e) l <$> goExpr r
      | Just _ <- matchRecordOp e = chain goExpr e
      | Just (ann, _, _, vals, nVals) <- matchRecordCall e = do
          -- Partial application: eta-expand, binding any non-trivial
          -- argument outside the lambda so it's still evaluated once.
          vals' <- traverse goExpr vals
          missing <- traverse (const (freshRecIdent "recArg")) [length vals + 1 .. nVals]
          named <- for vals' $ \v ->
            if trivial v
              then pure (v, Nothing)
              else do
                x <- freshRecIdent "recVal"
                pure (local ann x, Just (NonRec ann x v))
          let sat = rebuild e (map fst named ++ map (local ann) missing)
          body <- case sat of
            _ | Just (_, "get", [l], [r], _) <- matchRecordCall sat -> pure (Accessor ann l r)
            _ -> chain pure sat
          pure $ foldr (Let ann . pure) (foldr (Abs ann) body missing) (mapMaybe snd named)
      | otherwise = goExprOneLevel e

    local ann x = Var ann (Qualified ByNullSourcePos x)

    trivial = \case
      Var {} -> True
      Abs {} -> True
      Literal _ (ArrayLiteral _) -> False
      Literal _ (ObjectLiteral _) -> False
      Literal {} -> True
      _ -> False

    -- Replace a call's value arguments, keeping its function and dictionaries.
    rebuild e vals = case matchRecordCall e of
      Just (ann, _, labels, _, _) ->
        let (hd, args) = spine e []
         in foldl' (App ann) hd (take (length labels) args ++ vals)
      Nothing -> e

    spine (App _ f a) acc = spine f (a : acc)
    spine f acc = (f, acc)

    goBinder :: Binder Ann -> Supply (Binder Ann)
    goBinder = goBinderOneLevel

    goCaseAlt :: CaseAlternative Ann -> Supply (CaseAlternative Ann)
    goCaseAlt = goCaseAltOneLevel

    (goBind, goExprOneLevel, goBinderOneLevel, goCaseAltOneLevel) =
      traverseCoreFn goBind goExpr goBinder goCaseAlt

-- | Fuse a chain of record operations, `rec` being applied to every
-- sub-expression that is carried over into the result.
chain :: (Expr Ann -> Supply (Expr Ann)) -> Expr Ann -> Supply (Expr Ann)
chain rec e = do
  let (ops, base) = peel e []
  base' <- rec base
  ops' <- traverse (recOp rec) ops
  st <- foldM (step ann) (RecState base' Nothing [] []) ops'
  pure (flush ann st)
  where
    ann = extractAnn e
    peel x acc = case matchRecordOp x of
      Just (op, r) -> peel r (op : acc)
      Nothing -> (acc, x)

recOp :: (Expr Ann -> Supply (Expr Ann)) -> RecOp -> Supply RecOp
recOp rec = \case
  OpSet l v -> OpSet l <$> rec v
  OpModify l f -> OpModify l <$> rec f
  OpInsert l v -> OpInsert l <$> rec v
  OpPut fs -> OpPut <$> traverse (traverse rec) fs
  OpMergeOver l -> OpMergeOver <$> rec l
  op -> pure op

step :: Ann -> RecState -> RecOp -> Supply RecState
step ann st = \case
  OpId -> pure st
  OpSet l v -> pure st {stUps = upsert l Assign v (stUps st)}
  OpInsert l v
    | l `elem` stDels st -> pure st {stDels = filter (/= l) (stDels st), stUps = upsert l Assign v (stUps st)}
    | otherwise -> pure st {stUps = upsert l Insert v (stUps st)}
  OpPut fs -> pure $ foldl' (\s (l, v) -> s {stDels = filter (/= l) (stDels s), stUps = replaceKind l v (stUps s)}) st fs
  OpModify l f -> do
    (cur, st') <- current l st
    pure st' {stUps = upsert l Assign (App ann f cur) (stUps st')}
  OpDelete l -> pure (delete l st)
  OpRename from to -> do
    (cur, st') <- current from st
    step ann (delete from st') (OpInsert to cur)
  OpMergeOver l ->
    let r = flush ann st
     in pure (RecState (raw ann "maps:merge(($r), ($l))" [("r", r), ("l", l)]) Nothing [] [])
  where
    upsert l kind v ups
      | any (\(l', _, _) -> l' == l) ups = [if l' == l then (l', k, v) else u | u@(l', k, _) <- ups]
      | otherwise = ups ++ [(l, kind, v)]
    -- A merged-in value overrides the label without changing how it is written.
    replaceKind l v = upsert l Merge v

    delete l s = case [k | (l', k, _) <- stUps s, l' == l] of
      [Insert] -> s {stUps = [u | u@(l', _, _) <- stUps s, l' /= l]}
      _ -> s {stDels = l : stDels s, stUps = [u | u@(l', _, _) <- stUps s, l' /= l]}

    -- The current value of a label: a pending update's, or else read from the
    -- record (bound to a name first, as it is also the target of the update).
    current l s = case [v | (l', _, v) <- stUps s, l' == l] of
      v : _ -> pure (v, s)
      [] -> do
        (b, s') <- baseVar s
        pure (Accessor ann l b, s')

    baseVar s = case stBase s of
      b@(Var {}) -> pure (b, s)
      b -> do
        x <- freshRecIdent "recBase"
        let v = Var ann (Qualified ByNullSourcePos x)
        pure (v, s {stBase = v, stLet = Just (x, b)})

flush :: Ann -> RecState -> Expr Ann
flush ann st = maybe body (\(x, b) -> Let ann [NonRec ann x b] body) (stLet st)
  where
    body
      | null (stUps st) = withoutDels
      | otherwise = raw ann ("($b)#{" <> T.intercalate ", " (zipWith entry [1 :: Int ..] (stUps st)) <> "}") (("b", withoutDels) : [(valName i, v) | (i, (_, _, v)) <- zip [1 ..] (stUps st)])
    withoutDels = foldl' (\b l -> raw ann ("maps:remove(" <> atomText l <> ", ($b))") [("b", b)]) (stBase st) (stDels st)
    valName i = "v" <> T.pack (show (i :: Int))
    entry i (l, kind, _) = atomText l <> (if kind == Assign then " := $" else " => $") <> valName i

-- | A label as it is written as a key in Erlang source, quoted if need be.
-- `$` is escaped as it would otherwise start a template placeholder.
atomText :: PS.PSString -> Text
atomText = T.replace "$" "\\x24" . runAtom . AtomPS Nothing

-- | Emit Erlang source through the `CodeGen.erlang` escape hatch that purerl's
-- Inliner.hs turns into raw source, with `$name` standing for each binding.
raw :: Ann -> Text -> [(Text, Expr Ann)] -> Expr Ann
raw ann fmt binds =
  App ann (App ann (Var ann codeGenErlangIdent) (Literal ann (StringLiteral (PS.mkString fmt)))) (Literal ann (ObjectLiteral [(PS.mkString k, v) | (k, v) <- binds]))

expandAltChains :: [Bind Ann] -> Supply [Bind Ann]
expandAltChains = traverse goBind
  where
    goExpr :: Expr Ann -> Supply (Expr Ann)
    goExpr e
      | Just (kind, a, b) <- matchAlt2 e = do
          let chain = collectAltChain kind a <> collectAltChain kind b
          chain' <- traverse goExpr chain
          buildAltMaybeBlock (extractAnn e) kind chain'
      | otherwise = goExprOneLevel e

    goBinder :: Binder Ann -> Supply (Binder Ann)
    goBinder = goBinderOneLevel

    goCaseAlt :: CaseAlternative Ann -> Supply (CaseAlternative Ann)
    goCaseAlt = goCaseAltOneLevel

    (goBind, goExprOneLevel, goBinderOneLevel, goCaseAltOneLevel) =
      traverseCoreFn goBind goExpr goBinder goCaseAlt

-- | The chain as a call to `CodeGen.altChain`, which purerl's Erlang-level
-- optimizer turns into a flat `maybe` block (see inlineAltChain). The first
-- argument says which `Alt` instance the operands are from.
buildAltMaybeBlock :: Ann -> AltKind -> [Expr Ann] -> Supply (Expr Ann)
buildAltMaybeBlock ann kind operands =
  pure $
    App
      ann
      (App ann (Var ann altChainIdent) (Literal ann (StringLiteral (PS.mkString kindName))))
      (Literal ann (ObjectLiteral [(PS.mkString ("e" <> T.pack (show i)), e) | (i, e) <- zip [1 :: Int ..] operands]))
  where
    kindName = case kind of
      SimpleFail "{nothing}" -> "maybe"
      SimpleFail _ -> "either"
      VeitherKind -> "veither"

altChainIdent :: Qualified Ident
altChainIdent = Qualified (ByModuleName (ModuleName "CodeGen")) (Ident "altChain")

translateBacktrace :: ModuleName -> [Ident] -> [Bind Ann] -> [Bind Ann]
translateBacktrace modu foreignIdents binds =
  map transformBinds binds

  where
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs e =
    case e of
      App ann@(ss,ann2,Just IsSyntheticApp) fun (Var _ (Qualified (ByModuleName C.M_Backtrace) (Ident backtrace))) | T.isPrefixOf "backtrace" backtrace ->
        -- Debug.trace (show ("backtrace.transformExprs.hit", e)) $
        let erl a =
              App (ss, ann2, Nothing)
                (App (ss, ann2, Nothing)
                  (Var ann (Qualified (ByModuleName (ModuleName "CodeGen")) (Ident "erlang")))
                  (Literal ann (StringLiteral a))
                )
                (Literal ann (ObjectLiteral [])) in

        App ann fun $
        Literal ann $
          ObjectLiteral
            [ ("trace"
              , Literal ann
                (ObjectLiteral
                  [ ("?MODULE", erl "?MODULE")
                  , ("?FILE", erl "?FILE")
                  , ("?LINE", erl "?LINE")
                  , ("?FUNCTION_NAME", erl "?FUNCTION_NAME")
                  , ("?FUNCTION_ARITY", erl "?FUNCTION_ARITY")
                  , ("moduleName", Literal ann (StringLiteral (PS.fromText (runModuleName modu))))
                  , ("file", Literal ann (StringLiteral (PS.fromText (P.spanName ss))))
                  , ("spanStartLine", Literal ann (NumericLiteral (Left (toInteger (P.sourcePosLine (P.spanStart ss))))))
                  , ("spanStartColumn", Literal ann (NumericLiteral (Left (toInteger (P.sourcePosColumn (P.spanStart ss))))))
                  , ("spanStopLine", Literal ann (NumericLiteral (Left (toInteger (P.sourcePosLine (P.spanEnd ss))))))
                  , ("spanStopColumn", Literal ann (NumericLiteral (Left (toInteger (P.sourcePosColumn (P.spanEnd ss))))))
                  ]
                )
              )
            ]

      _ -> e


optimizeModuleDecls :: ModuleName -> [Ident] -> [Bind Ann] -> [Bind Ann]
optimizeModuleDecls modu foreignIdents binds =
  map transformBinds $
    force $
      map (
       renameIdentsAndVars modu
        ( -- Debug.trace (show ("renameIdentsAndVars-pre", modu)) $
          Map.fromList $
            map (,())
            (concatMap bindIdents binds <>
            foreignIdents
            )
        )
      ) binds

  where
  -- (handleBind, handleExprDefault, handleBinder, _) = traverseCoreFn handleBind handleExpr handleBinder handleCaseAlternative
  -- (uniqueNamedBinds, uniqueExprs, uniqueBinder, uniqueCase) = traverseCoreFn (p uniqueNamedBinds) (p uniqueExprs) (p uniqueBinder) (p uniqueCase)
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs
    = optimizeDataFunctionApply

  p f a =
    do
      traceM (show a)
      f a
{-
  printExprs x =
    do
      traceM (show x)
      uniqueExprs case x of
        Let a binds rhs ->
          do
            renamedBinds <- mapM regBind binds
            pure (Let a renamedBinds rhs)
        Abs a ident rhs ->
          do
            rIdent <- renameIdent ident
            pure (Abs a rIdent rhs)
        Var a qual ->
          do
            rqual <- renameVar qual
            pure (Var a rqual)
        Constructor a tname cname idents ->
          do
            ridents <- mapM renameIdent idents
            pure (Constructor a tname cname ridents)
        _ ->
          pure x
  f b =
    unsafePerformIO $ uniqueNamedBinds b
    -- case b of
    --   NonRec _ (Ident "zf") _ ->
    --     unsafePerformIO $
    --       uniqueNamedBinds b
    --   _ ->
    --     uniqueNamedBinds b
    --     b
-}

optimizeDataFunctionApply :: Expr a -> Expr a
optimizeDataFunctionApply e =
  case e of
    (App a (App _ (Var _ fn) x) y)
      | C.I_functionApply <- fn -> App a x y
      | C.I_functionApplyFlipped <- fn -> App a y x
-- {-
    (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorArray <- impl -> Var a C.I_arrayMap
    (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorMaybe <- impl -> Var a C.I_maybeMap
    (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorEither <- impl -> Var a C.I_eitherMap
    -- (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorList <- impl -> Var a C.I_listMap -- TODO[drathier]: impl is typeclassy; reimplement without type class or we'll get an inf loop here
    -- (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorEffect <- impl -> Var a C.I_eMap -- TODO[drathier]: implementation in E.purs is just Functor.map, so inf loop if we do this rewrite. Effect.purs doesn't expose the function. UnsafePerformEffect doesn't seem pretty enough. Implement it in FFI instead.

    -- TODO[drathier]: Bind takes two args at once sometimes, so blocked on another stab at the exact-call optimization
    -- -- (control_bind@ps:bind((control_bind@ps:bindArray())))
    -- (App a (Var b fn) (Var c impl)) | C.I_bind <- fn, C.I_bindArray <- impl -> Var a C.I_arrayBind
    -- -- (control_bind@ps:bind((maybe@ps:bindMaybe())))
    -- (App a (Var b fn) (Var c impl)) | C.I_bind <- fn, C.I_bindMaybe <- impl -> Var a C.I_bindMaybe
    -- -- (control_bind@ps:bind((either@ps:bindEither())))
    -- (App a (Var b fn) (Var c impl)) | C.I_bind <- fn, C.I_bindEither <- impl -> Var a C.I_eitherBind
    -- (control_bind@ps:bind((effect@ps:bindEffect())))
    -- (App a (Var b fn) (Var c impl)) | C.I_bind <- fn, C.I_bindEffect <- impl -> Var a C.I_effectBindE
    -- (App a (Var b fn) (Var c impl)) | C.I_eBind <- fn, C.I_bindEffect <- impl -> Var a C.I_effectBindE



    -- (control_applicative@ps:pure((control_applicative@ps:applicativeArray())))
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeArray <- impl -> Var a C.I_arrayPure
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeMaybe <- impl -> Var a C.I_maybePure
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeEither <- impl -> Var a C.I_eitherPure
    -- (control_applicative@ps:pure((effect@ps:applicativeEffect())))
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeEffect <- impl -> Var a C.I_effectPureE

-- -}
    _ -> e


-------------------------
-- NOTE[drathier]: core idea: traverse ast top-down (in variable binding order), registering variables for renaming as we go without renaming them, and then translating all idents including the ones in e.g. the left side of a let bind. `let a = a+1` registers the `a => a42` mapping (to a unique var), then when it gets to each of the `a`'s, they'll get renamed as idents using the `a => a42` mapping.

-- -- Assuming Ident is defined as:
-- newtype Ident = Ident Text deriving (Show, Eq, Ord)

-- State contains a counter for generating unique names and an environment for scoping
type RenameState = (Int, Map Ident Ident, Map Ident (), Text, Bool)

-- Look up an identifier in the environment, defaulting to the original name
lookupIdent :: Ident -> State RenameState Ident
lookupIdent ident = do
  (_, env, _, prefix, isCtor) <- get
  -- Debug.trace (show ("CoreFn.Optimizer.lookupIdent Map.lookup Just", ident, env)) $
  -- TODO[drathier]: we'll likely have to either stop returning idents here and only rely on recursion schemes to translate idents after registering them here, or add identity mappings for already translated idents
  -- Debug.trace (show ("CoreFn.Optimizer.lookupIdent Map.lookup Nothing", prefix, ident, env)) $
  pure $ fromMaybe ident ((if isCtor then Just (Ident (prefix <> runIdent ident)) else Nothing) <|> Map.lookup ident env)
  -- pure ident

-- Render a per-bind-local counter as a short, rolling letter name: A, B, ..., Z, AA, AB, ...
-- (bijective base-26, like spreadsheet column names). The counter alone already guarantees
-- uniqueness within a top-level bind's rename pass, so no other qualifier is needed.
counterToLetters :: Int -> Text
counterToLetters n = go (n + 1) []
  where
  go 0 acc = T.pack acc
  go k acc =
    let (k', r) = (k - 1) `divMod` 26
    in go k' (chr (ord 'A' + r) : acc)

-- Extend the environment with a new mapping for a bound variable. Returns the input Ident for convenience in <$> <*> chains. The name will be replaced when the inner ident is processed.
extendEnv :: Ident -> State RenameState Ident
extendEnv old = do
  (counter, env, ignores, prefix, isCtor) <- get
  case Map.member old ignores of
    True ->
      -- (\res -> Debug.trace (show ("extendEnv ignored", old, "res", res)) res) <$>
      pure old
    False -> do
      let new = Ident (counterToLetters counter <> "_" <> runIdent old)
      -- (\res -> Debug.trace (show ("extendEnv change", old, counter, new, "res", res)) res) <$>
      do
        -- Debug.traceM (show ("CoreFn.Optimizer.extendEnv", old, new, counter, env))
        put (counter + 1, Map.insert old new env, ignores, prefix, isCtor)
        pure old


renameIdentsAndVars :: forall a. NFData a => Show a => ModuleName -> Map Ident () -> Bind a -> Bind a
renameIdentsAndVars modu topLevelFunctions bind =
  -- (\res -> Debug.trace (show ("renameIdentsAndVars-after", bind, "res", res)) res) $
  let
    isolated :: forall b. State RenameState b -> State RenameState b
    isolated inner = do
      (counter, vars, ignores, prefix, isCtor) <- get
      res <- inner
      (counter2, _, _, _, _) <- get
      put (counter2, vars, ignores, prefix, isCtor)
      pure res

    extendPrefix :: forall b. Ident -> State RenameState b -> State RenameState b
    extendPrefix ident inner = do
      (counter1, vars1, ignores1, prefix1, isCtor1) <- get
      put (counter1, vars1, ignores1, prefix1 <> runIdent ident <> "_", isCtor1)
      res <- inner
      (counter2, vars2, ignores2, _, isCtor2) <- get
      put (counter2, vars2, ignores2, prefix1, isCtor2)
      pure res

    -- Rename identifiers at usage sites
    onIdent :: Ident -> State RenameState Ident
    onIdent ident = do
      -- (\res -> Debug.trace (show ("onIdent", ident, "res", res)) res) <$>
      do
        (_, _, ignores, _, _) <- get
        case Map.member ident ignores of
          True ->
            pure ident
          False ->
            lookupIdent ident

    -- Handle binders in CaseAlternatives
    onCaseAlt :: CaseAlternative a -> State RenameState (CaseAlternative a)
    onCaseAlt caseAlt =
      -- (\res -> Debug.trace (show ("onCaseAlt", caseAlt, "res", res)) res) <$>
      -- traverseCaseAlt caseAlt
      pure caseAlt
      -- case caseAlt of
      --   CaseAlternative binders result -> do
      --     newBinders <- traverse (\binder -> onBinder binder >>= onBinder) binders
      --     let goResult (Left guards) = Left <$> traverse (\(guard, expr) -> (,) <$> onExpr guard <*> onExpr expr) guards
      --         goResult (Right expr) = Right <$> onExpr expr
      --     extendEnv binders newBinders $ CaseAlternative newBinders <$> goResult result

    onBind :: Bind a -> State RenameState (Bind a)
    onBind b =
      -- [drathier]: register new idents, and rely on recursion scheme to rename idents according to the new names right afterwards
      -- (\res -> Debug.trace (show ("onBind", b, "res", res)) res) <$>
      -- traverseBind =<<
        case b of
          NonRec ann ident expr -> do
            extendEnv ident
            pure b
          Rec bindings -> do
            traverse_ (extendEnv . snd . fst) bindings
            pure b

    onBinder :: Binder a -> State RenameState (Binder a)
    onBinder binder =
      -- (\res -> Debug.trace (show ("onBinder", binder, "res", res)) res) <$>
      -- traverseBinder =<<
        case binder of
          VarBinder ann ident -> do
            extendEnv ident
            pure binder
          NamedBinder ann ident _innerBinder -> do
            extendEnv ident
            pure binder
          _ -> pure binder

    onExpr :: Expr a -> State RenameState (Expr a)
    onExpr e =
      -- (\res -> Debug.trace (show ("onExpr", e, "res", res)) res) <$>
      -- traverseExpr =<<
        case e of
          Abs a ident expr -> Abs a <$> extendEnv ident <*> pure expr
          _ -> pure e

    goBind :: Bind a -> State RenameState (Bind a)
    goBind b =
      onBind b >>=
      \case
        NonRec ann ident expr -> do
          NonRec <$> pure ann <*> goIdent ident <*> extendPrefix ident (goExpr expr)
        Rec bindings -> do
          Rec <$>
            traverse (\((ann, ident), expr) -> do
              isolated $ (,) <$> ((,) <$> pure ann <*> goIdent ident) <*> extendPrefix ident (goExpr expr)
            ) bindings

    goExpr :: Expr a -> State RenameState (Expr a)
    goExpr expr =
      onExpr expr >>=
      \case
        Literal ann lit ->
          Literal <$> pure ann <*> goLitExpr lit
        Constructor ann typeName ctorName fields -> do
          s@(a,b,c,d,isCtor) <- get
          put (a,b,c,d,True)
          res <- Constructor <$> pure ann <*> pure typeName <*> pure ctorName <*> traverse goIdent fields
          put (a,b,c,d,isCtor)
          pure res
        Accessor ann prop expr ->
          Accessor <$> pure ann <*> pure prop <*> goExpr expr
        ObjectUpdate ann obj mCopy fields ->
          ObjectUpdate <$> pure ann <*> goExpr obj <*> pure mCopy <*> traverse (\(k, v) -> (,) <$> pure k <*> goExpr v) fields
        Abs ann ident body ->
          Abs <$> pure ann <*> goIdent ident <*> goExpr body
        App ann fn arg ->
          App <$> pure ann <*> goExpr fn <*> goExpr arg
        Var ann qIdent ->
          Var <$> pure ann <*> goQIdent qIdent
        Case ann cases alts ->
          Case <$> pure ann <*> traverse goExpr cases <*> traverse goCaseAlt alts
        Let ann binds body ->
          Let <$> pure ann <*> traverse goBind binds <*> goExpr body

    goQIdent :: Qualified Ident -> State RenameState (Qualified Ident)
    goQIdent qi =
        case qi of
          -- Qualified (BySourcePos spos) a ->
          -- [drathier]: stop recursing on external module, so we don't rename those qidents
          Qualified (ByModuleName qmodu) a | qmodu /= modu -> pure qi
          Qualified qualifiedBy a ->
            -- [drathier]: internal modules are recursed into, and translated. We could translate them here directly, and make the code a bit easier to read
            Qualified <$> pure qualifiedBy <*> goIdent a

    goBinder :: Binder a -> State RenameState (Binder a)
    goBinder b =
      onBinder b >>=
      \case
        NullBinder ann ->
          pure $ NullBinder ann
        LiteralBinder ann lit ->
          LiteralBinder <$> pure ann <*> goLitBinder lit
        VarBinder ann ident ->
          VarBinder <$> pure ann <*> goIdent ident
        ConstructorBinder ann qType qCtor binders ->
          ConstructorBinder <$> pure ann <*> pure qType <*> pure qCtor <*> traverse goBinder binders
        NamedBinder ann ident binder ->
          NamedBinder <$> pure ann <*> goIdent ident <*> goBinder binder

    goCaseAlt :: CaseAlternative a -> State RenameState (CaseAlternative a)
    goCaseAlt c =
      isolated $
      onCaseAlt c >>=
      \(CaseAlternative binders result) -> CaseAlternative <$> traverse goBinder binders <*> goResult result
      where
        goResult (Left guards) = Left <$> traverse (\(guard, expr) -> (,) <$> goExpr guard <*> goExpr expr) guards
        goResult (Right expr) = Right <$> goExpr expr

    goLitExpr :: Literal (Expr a) -> State RenameState (Literal (Expr a))
    goLitExpr le =
      onLitExpr le >>=
      \case
        NumericLiteral n ->
          pure $ NumericLiteral n
        StringLiteral s ->
          pure $ StringLiteral s
        CharLiteral c ->
          pure $ CharLiteral c
        BooleanLiteral b ->
          pure $ BooleanLiteral b
        ArrayLiteral xs ->
          ArrayLiteral <$> traverse goExpr xs
        ObjectLiteral fields ->
          ObjectLiteral <$> traverse (\(k, v) -> (,) <$> pure k <*> goExpr v) fields

    goLitBinder :: Literal (Binder a) -> State RenameState (Literal (Binder a))
    goLitBinder lb =
      onLitBinder lb >>=
      \case
        NumericLiteral n ->
          pure $ NumericLiteral n
        StringLiteral s ->
          pure $ StringLiteral s
        CharLiteral c ->
          pure $ CharLiteral c
        BooleanLiteral b ->
          pure $ BooleanLiteral b
        ArrayLiteral xs ->
          ArrayLiteral <$> traverse goBinder xs
        ObjectLiteral fields ->
          ObjectLiteral <$> traverse (\(k, v) -> (,) <$> pure k <*> goBinder v) fields

    onLitBinder = pure
    onLitExpr = pure

    goIdent :: Ident -> State RenameState Ident
    goIdent = onIdent
  in
  force $ evalState (goBind bind)--(Debug.trace (show ("renameIdentsAndVars-startbind", modu, bind)) $ bind))
    ( 0
    , Map.empty
    , topLevelFunctions <> Map.fromList (map (,()) (bindIdents bind)) <> unusedVar
    , runModuleName modu <> "_"
    , False
    )

unusedVar =
  -- NOTE[drathier]: this is needed to reduce the number of unique persistent term keys in type class dict memoization. Erlang allows `_` to be rebound (but not _ prefixed vars).
  Map.fromList [(UnusedIdent, ())]
