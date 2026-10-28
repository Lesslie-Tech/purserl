{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- This module generates code in the simplified Erlang intermediate representation from Purescript code
module Language.PureScript.Erl.CodeGen
  ( module AST,
    moduleToErl,
    buildCodegenEnvironment,
    CodegenEnvironment,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Monad (foldM, replicateM, unless)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Supply.Class (MonadSupply (fresh), bumpToNextRoundNumber)
import Language.PureScript.Erl.Make.Monad (Make)
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT (runWriterT))
import Data.Either (fromRight)
import Data.Foldable (find, traverse_, foldl')
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (forM)
import Debug.Trace (trace, traceM)
-- import qualified Language.PureScript as P
import Language.PureScript.AST (SourceSpan, nullSourceSpan)
import qualified Language.PureScript.Constants.Libs as C
import qualified Language.PureScript.Constants.Prim as C
import Language.PureScript.CoreFn
  ( Ann,
    Bind (..),
    Binder (..),
    CaseAlternative (CaseAlternative, caseAlternativeBinders),
    Expr (..),
    Literal (..),
    Meta (IsConstructor, IsNewtype, IsTypeClassConstructor, IsSyntheticApp),
    Module (Module),
    everywhereOnValues,
    extractAnn,
    ssAnn,
  )
import Language.PureScript.Environment as E
  ( Environment (names, typeClasses, typeSynonyms, types),
    TypeClassData (typeClassIsEmpty),
    tyFunction,
  )
import Language.PureScript.Erl.CodeGen.AST as AST
-- import Language.PureScript.Erl.CodeGen.CheckedWrapper (typecheckWrapper)
import Language.PureScript.Erl.CodeGen.Common
  ( ModuleType (ForeignModule, PureScriptModule),
    atomModuleName,
    freshNameErl,
    freshNameErl',
    identToVar,
    toAtomName, identToAtomName, runIdent', runAtom
  )
import Language.PureScript.Erl.CodeGen.Constants.PureScriptModules
  ( dataFunctionUncurried,
    effectUncurried,
  )
import Language.PureScript.Erl.Errors (MultipleErrors, addHint, errorMessage, rethrow, rethrowWithPosition)
import Language.PureScript.Erl.Errors.Types
  ( SimpleErrorMessage
      ( InvalidFFIArity,
        MissingFFIImplementations,
        UnusedFFIImplementations
      ),
  )
import Language.PureScript.Erl.Synonyms (replaceAllTypeSynonyms')
import Language.PureScript.Errors (ErrorMessageHint (..))
import Language.PureScript.Names
  ( Ident (Ident, UnusedIdent),
    ModuleName (..),
    ProperName (ProperName),
    Qualified (..)
  )
import Language.PureScript.Options (Options)
import Language.PureScript.Traversals (sndM)
import Language.PureScript.Types
  ( Constraint (constraintClass),
    SourceType,
    Type (..),
  )
import Prelude.Compat
import Language.PureScript.Erl.CodeGen.Types (ETypeEnv, uncurriedFnTypes, replaceVars, translateType, uncurryType)
-- import Language.PureScript (internalError)
-- ### purserl

-- import qualified Language.PureScript as P
import qualified Control.Monad.Supply as P
import qualified Data.Version (Version)
import qualified Language.PureScript.AST as P
import qualified Language.PureScript.Comments as P
import qualified Language.PureScript.Crash as P
import qualified Language.PureScript.Environment as P
import qualified Language.PureScript.Errors as P hiding (indent)
import qualified Language.PureScript.Externs as P
import qualified Language.PureScript.Linter as P
import qualified Language.PureScript.ModuleDependencies as P
import qualified Language.PureScript.Names as P
import qualified Language.PureScript.Options as P
import qualified Language.PureScript.Pretty as P
import qualified Language.PureScript.Renamer as P
import qualified Language.PureScript.Roles as P
import qualified Language.PureScript.Sugar as P
import qualified Language.PureScript.TypeChecker as P
import qualified Language.PureScript.Types as P

-- import Language.PureScript (internalError)
import Language.PureScript.Crash (internalError)
--
import Language.PureScript.CoreFn.Expr qualified as E
import Control.Monad.State.Strict (StateT(..), runStateT, mapStateT, lift, get, put, modify)

import Language.PureScript.PSString qualified as PS
import Language.PureScript.AST.SourcePos qualified as SS

identToTypeclassCtor :: Ident -> Atom
identToTypeclassCtor a = Atom Nothing (runIdent' a)

qualifiedToErl' :: ModuleName -> ModuleType -> Ident -> Atom
qualifiedToErl' mn' moduleType ident = Atom (Just $ atomModuleName mn' moduleType) (runIdent' ident)

-- Top level definitions are everywhere fully qualified, variables are not.
qualifiedToErl :: ModuleName -> Qualified Ident -> Atom
qualifiedToErl mn (Qualified (P.ByModuleName mn') ident)
  -- Local reference to local non-exported function
  | mn == mn' -- TODO making all local calls non qualified - for memoization onload - revert
  -- && ident `Set.notMember` declaredExportsSet  =
    =
    Atom Nothing (runIdent' ident)
  -- Reference other modules or exported things via module name
  | otherwise =
    qualifiedToErl' mn' PureScriptModule ident
qualifiedToErl _ (Qualified (P.BySourcePos _) ident) = Atom Nothing (runIdent' ident)




uncurriedFnArity :: ModuleName -> T.Text -> SourceType -> Maybe Int
uncurriedFnArity moduleName fnName ty = fst <$> uncurriedFnTypes moduleName fnName ty

concatRes :: [([a], [b], ETypeEnv)] -> ([a], [b], ETypeEnv)
concatRes x = (concatMap (\(a, _, _) -> a) x, concatMap (\(_, b, _) -> b) x, M.unions $ (\(_, _, c) -> c) <$> x)

isFullySaturatedForeignCall :: ModuleName -> M.Map (Qualified Ident) Int -> Expr Ann -> [a] -> Bool
isFullySaturatedForeignCall mn actualForeignArities var args = case var of
  Var _ qi
    | P.isQualifiedWith mn qi,
      Just arity <- M.lookup qi actualForeignArities,
      length args == arity ->
      True
  _ -> False


data FnArity = EffFnXArity Int | FnXArity Int | Arity (Int, Int)
  deriving (Eq, Show)

-- A typeclass with zero function members (and only empty-member superclasses)
-- never gets a real dictionary argument anymore -- the type checker omits it
-- entirely (see Entailment's Unused handling and CoreFn.Desugar's dropping of
-- App-to-Unused). Arity computed from the type signature has to skip these
-- constraints too, to stay consistent with CoreFn's actual (reduced) shape.
isEmptyClassConstraint :: E.Environment -> Constraint a -> Bool
isEmptyClassConstraint env con =
  maybe False typeClassIsEmpty $ M.lookup (constraintClass con) (E.typeClasses env)

data CodegenEnvironment = CodegenEnvironment E.Environment (M.Map (Qualified Ident) FnArity)

buildCodegenEnvironment :: E.Environment -> CodegenEnvironment
buildCodegenEnvironment env = CodegenEnvironment env explicitArities
  where
    tyArity :: SourceType -> FnArity
    tyArity t = Arity $ go 0 t'
      where
        t' = fromRight t $ replaceAllTypeSynonyms' (E.typeSynonyms env) (E.types env) t

        go n = \case
          ConstrainedType _ con ty -> go (if isEmptyClassConstraint env con then n else n + 1) ty
          ForAll _ _ _ _ ty _ -> go n ty
          other -> (n, go' other)
        go' = \case
          TypeApp _ (TypeApp _ fn _) ty | fn == E.tyFunction -> 1 + go' ty
          ForAll _ _ _ _ ty _ -> go' ty
          _ -> 0

    explicitArities :: M.Map (Qualified Ident) FnArity
    explicitArities = tyArity <$> types

    types :: M.Map (Qualified Ident) SourceType
    types = M.map (\(t, _, _) -> t) $ E.names env

data Arities = Arities
  { _effective :: M.Map (Qualified Ident) FnArity
  , _actualForeign :: M.Map (Qualified Ident) Int
  }

findArities :: ModuleName -> [Bind Ann] -> CodegenEnvironment -> [(T.Text, Int)] -> Arities
findArities mn decls (CodegenEnvironment _ explicitArities) foreignExports = Arities arities actualForeignArities
  where
  actualForeignArities :: M.Map (Qualified Ident) Int
  actualForeignArities = M.fromList $ map (\(x, n) -> (Qualified (P.ByModuleName mn) (Ident x), n)) foreignExports

  arities :: M.Map (Qualified Ident) FnArity
  arities =
    -- max arities is max of actual impl and most saturated application
    let inferredArities = foldr findUsages (Set.singleton <$> actualForeignArities) decls
        inferredMaxArities = M.mapMaybe (fmap (\n -> Arity (0, n)) . Set.lookupMax) inferredArities
      in explicitArities `M.union` inferredMaxArities

  findUsages :: Bind Ann -> M.Map (Qualified Ident) (Set Int) -> M.Map (Qualified Ident) (Set Int)
  findUsages (NonRec _ _ val) apps = findUsages' val apps
  findUsages (Rec vals) apps = foldr (findUsages' . snd) apps vals

  findUsages' :: Expr Ann -> M.Map (Qualified Ident) (Set Int) -> M.Map (Qualified Ident) (Set Int)
  findUsages' expr apps = case expr of
    e@App {} ->
      let (f, args) = unApp e []
          apps' = foldr findUsages' apps args
        in case f of
            Var (_, _, Just IsNewtype) _ -> apps'
            -- This is an app but we inline these
            Var (_, _, Just (IsConstructor _ fields)) (Qualified _ _)
              | length args == length fields ->
                apps'
            Var (_, _, Just IsTypeClassConstructor) _ ->
              apps'
            -- Don't count fully saturated foreign import call, it will be called directly
            Var _ _
              | isFullySaturatedForeignCall mn actualForeignArities f args ->
                apps'
            Var _ (Qualified q ident)
              | thisModule q ->
                M.insertWith Set.union (Qualified (P.ByModuleName mn) ident) (Set.singleton $ length args) apps'
            _ -> findUsages' f apps'
    v@Var {} ->
      case v of
        -- Must actually not assume this is 0 as it may be a ref fn/1 or fnx/efffnx/n
        -- Should record separately - or assume that 0 may mean non-0 for this reason?
        Var _ (Qualified q ident)
          | thisModule q ->
            M.insertWith Set.union (Qualified (P.ByModuleName mn) ident) (Set.singleton 0) apps
        _ -> apps
    Accessor _ _ e -> findUsages' e apps
    ObjectUpdate _ e _ es -> findUsages' e $ foldr (findUsages' . snd) apps es
    Abs _ _ e -> findUsages' e apps
    Case _ e es -> foldr findUsages' (foldr findUsagesCase apps es) e
    Let _ b e' ->
      findUsages' e' $ foldr findUsages'' apps b
    Literal _ litExpr -> case litExpr of
      NumericLiteral _ -> apps
      StringLiteral _ -> apps
      CharLiteral _ -> apps
      BooleanLiteral _ -> apps
      ArrayLiteral exprs -> foldr findUsages' apps exprs
      ObjectLiteral fields -> foldr (findUsages' . snd) apps fields
    Constructor {} -> apps
    where
      unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
      unApp (App _ val arg) args = unApp val (arg : args)
      unApp other args = (other, args)

      thisModule (P.ByModuleName mn') = mn' == mn
      thisModule (P.BySourcePos _) = True

  findUsages'' (NonRec _ _ e) apps = findUsages' e apps
  findUsages'' (Rec binds) apps = foldr (findUsages' . snd) apps binds

  findUsagesCase (CaseAlternative _ (Right e)) apps = findUsages' e apps
  findUsagesCase (CaseAlternative _ (Left ges)) apps = foldr findUsages' apps $ concatMap (\(a, b) -> [a, b]) ges


-- |
-- Generate code in the simplified Erlang intermediate representation for all declarations in a
-- module.
type ErlM = P.SupplyT Make

type ErlW = WriterT MultipleErrors ErlM

moduleToErl ::
  CodegenEnvironment ->
  Module Ann ->
  [(T.Text, Int)] ->
  ErlM ([(Atom, Int)], [Erl], [Erl], [Erl], [(Atom, Int)], [Erl]) -- , Map Atom Int)
moduleToErl codegenEnv m@(Module _ _ mn _ _ _ _ _ _) foreignExports =
  rethrow (addHint (ErrorInModule mn)) $ do
    (res, warnings) <- runWriterT $ moduleToErl' codegenEnv m foreignExports
    tell warnings
    pure res

moduleToErl' ::
  CodegenEnvironment ->
  Module Ann ->
  [(T.Text, Int)] ->
  ErlW ([(Atom, Int)], [Erl], [Erl], [Erl], [(Atom, Int)], [Erl]) -- , Map Atom Int)
moduleToErl' cgEnv@(CodegenEnvironment env explicitArities) (Module _ _ mn _ _ declaredExports _ foreigns origDecls) foreignExports =
  do
    res <- traverse (\b ->
      do
        bumpToNextRoundNumber
        topBindToErl b
      ) decls
    reexports <- traverse reExportForeign foreigns
    let foreignSpecs = []

        (exports, erlDecls, _typeEnv) = concatRes $ res <> map (\(a, b, _, d) -> (a, b, d)) reexports
        namedSpecs = []

    traverse_ checkExport foreigns
    let usedFfi = Set.fromList $ map runIdent' foreigns
        definedFfi = Set.fromList (map fst foreignExports)
        unusedFfi = definedFfi Set.\\ usedFfi
    unless (Set.null unusedFfi) $
      tell (errorMessage $ UnusedFFIImplementations mn (Ident <$> Set.toAscList unusedFfi))

    let attributes = findAttributes decls

    -- safeDecls <- concat <$> traverse (typecheckWrapper mn) erlDecls
    let safeDecls = mempty


    let fnl (EFunctionDef _ _ fnName args _) = Just (fnName, length args)
        fnl _ = Nothing
        safeExports = mapMaybe fnl safeDecls

        -- memoizable =
        --   M.mapKeys (qualifiedToErl mn) $
        --     M.mapMaybe
        --       ( \case
        --           Arity (n, _) | n > 0 -> Just n
        --           _ -> Nothing
        --       )
        --       arities
        --       -- Var _ qi@(Qualified _ _)
    return (exports, namedSpecs, foreignSpecs, attributes ++ erlDecls, safeExports, safeDecls)
  where
    declaredExportsSet = Set.fromList declaredExports

    Arities arities actualForeignArities = findArities mn decls cgEnv foreignExports

    decls :: [Bind Ann]
    decls = go <$> origDecls
      where
        go (NonRec ann ident val) = NonRec ann ident (removeDollars val)
        go (Rec vals) = Rec $ map (second removeDollars) vals

        removeDollars = fe
          where
            (_, fe, _) = everywhereOnValues id go id

            go (App ann (App _ (Var _ apply) f) a)
              | C.I_functionApply <- apply = App ann f a
            go (App ann (App _ (Var _ apply) a) f)
              | C.I_functionApplyFlipped <- apply = App ann f a
            go other = other

    types :: M.Map (Qualified Ident) SourceType
    types = M.map (\(t, _, _) -> t) $ E.names env

    findAttributes :: [Bind Ann] -> [Erl]
    findAttributes expr = map (uncurry EAttribute) $ concatMap (mapMaybe getAttribute . onBind) expr
      where
        getAttribute (TypeApp _ (TypeApp _ (TypeConstructor _ (Qualified (P.ByModuleName _) (ProperName "Attribute"))) (TypeLevelString _ a)) (TypeLevelString _ b)) =
          Just (a, b)
        getAttribute _ = Nothing

        getType ident = M.lookup (Qualified (P.ByModuleName mn) ident) types

        onRecBind ((_, ident), _) = getType ident
        onBind (NonRec _ ident _) = catMaybes [getType ident]
        onBind (Rec vals) = mapMaybe onRecBind vals



    -- 're-export' foreign imports in the @ps module - also used for internal calls for non-exported foreign imports
    reExportForeign :: Ident -> ErlW ([(Atom, Int)], [Erl], Maybe (Ident, EType), ETypeEnv)
    reExportForeign ident = do
      let arity = exportArity ident
          fullArity = case M.lookup (Qualified (P.ByModuleName mn) ident) arities of
            Just (Arity (0, n)) -> n
            _ -> arity
          wrapTy (ty', tenv) = case arity of
            0 -> Just (TFun [] ty', tenv)
            _ -> (,tenv) <$> uncurryType arity ty'

          ffiTyEnv = wrapTy . translateType env =<< M.lookup (Qualified (P.ByModuleName mn) ident) types

      args <- replicateM fullArity freshNameErl
      let body = EApp RegularApp (EAtomLiteral $ qualifiedToErl' mn ForeignModule ident) (take arity $ map EVar args)
          body' = curriedApp (drop arity $ map EVar args) body
          fun = curriedLambda body' args
      fident <- fmap (Ident . ("f" <>) . T.pack . show) fresh
      let var = Qualified P.ByNullSourcePos fident
          -- wrap e = EBlock [EVarBind (identToVar fident) fun, e]
          wrap e = ELet (EVarBind (identToVar fident) fun) e
      (idents, erl, env) <- generateFunctionOverloads Nothing (ssAnn nullSourceSpan) ident (Atom Nothing $ runIdent' ident) (Var (ssAnn nullSourceSpan) var) wrap
      let combinedTEnv = M.union env (maybe M.empty snd ffiTyEnv)
      pure (idents, erl, (ident,) . fst <$> ffiTyEnv, combinedTEnv)



    exportArity :: Ident -> Int
    exportArity ident = fromMaybe 0 $ findExport $ runIdent' ident

    checkExport :: Ident -> ErlW ()
    checkExport ident =
      case (findExport (runIdent' ident), M.lookup (Qualified (P.ByModuleName mn) ident) explicitArities) of
        -- TODO is it meaningful to check against inferred arities (as we are just now) or only explicit ones
        -- This probably depends on the current codegen

        -- If we know the foreign import type (because it was exported) and the actual FFI type, it is an error if
        -- the actual implementation has higher arity than the type does
        -- If the actual implementation has lower arity, it may be just returning a function
        (Just m, Just (Arity (nc, n)))
          | m > nc + n ->
            throwError . errorMessage $ InvalidFFIArity mn (runIdent' ident) m (nc + n)
        -- If we don't know the declared type of the foreign import (because it is not exported), then we cannot say
        -- what the implementation's arity should be, as it may be higher than can be inferred from applications in this
        -- module (because there is no fully saturated application) or lower (because the ffi returns a function)
        (Just _, Nothing) ->
          pure ()
        -- We certainly can tell if an import exists and the implementation isn't found
        -- The opposite situation is handled at the top level of moduleToErl
        (Nothing, _) ->
          throwError . errorMessage $ MissingFFIImplementations mn [ident]
        _ -> pure ()

    findExport :: T.Text -> Maybe Int
    findExport n = snd <$> find ((n ==) . fst) foreignExports

    isTopLevelBinding :: Qualified Ident -> Bool
    isTopLevelBinding (Qualified (P.ByModuleName _) _) = True
    isTopLevelBinding (Qualified (P.BySourcePos _) ident) = ident `elem` topLevelNames

    topLevelNames = concatMap topLevelName decls
      where
        topLevelName :: Bind Ann -> [Ident]
        topLevelName (NonRec _ ident _) = [ident]
        topLevelName (Rec vals) = map (snd . fst) vals

    uncurriedFnArity' :: (Int -> FnArity) -> ModuleName -> T.Text -> Qualified Ident -> Maybe FnArity
    uncurriedFnArity' ctor fnMod fn ident =
      case M.lookup ident types of
        Just t -> ctor <$> uncurriedFnArity fnMod fn t
        _ -> Nothing

    effFnArity = uncurriedFnArity' EffFnXArity effectUncurried "EffectFn"
    fnArity = uncurriedFnArity' FnXArity dataFunctionUncurried "Fn"

    topBindToErl :: (Bind Ann -> ErlW ([(Atom, Int)], [Erl], ETypeEnv))
    topBindToErl = \case
      NonRec ann ident val -> topNonRecToErl ann ident val
      Rec vals -> concatRes <$> traverse (uncurry . uncurry $ topNonRecToErl) vals

    topNonRecToErl :: Ann -> Ident -> Expr Ann -> ErlW ([(Atom, Int)], [Erl], ETypeEnv)
    topNonRecToErl (ss, _, _) ident val = do
      let eann@(_, _, meta') = extractAnn val
          ident' = case meta' of
            Just IsTypeClassConstructor -> identToTypeclassCtor ident
            _ -> Atom Nothing $ runIdent' ident

      val' <- ensureFreshVars_ val
      generateFunctionOverloads (Just ss) eann ident ident' val' id

    generateFunctionOverloads :: Maybe SourceSpan -> Ann -> Ident -> Atom -> Expr Ann -> (Erl -> Erl) -> ErlW ([(Atom, Int)], [Erl], ETypeEnv)
    generateFunctionOverloads ss eann ident ident' val outerWrapper = do
      -- Always generate the plain curried form, f x y = ... -~~> f() -> fun (X) -> fun (Y) -> ... end end.
      let qident = Qualified (P.ByModuleName mn) ident

      erl <- valueToErl val

      let translated = translateType env <$> M.lookup qident types
          erlangType = replaceVars . fst <$> translated
          etypeEnv = maybe M.empty snd translated

      let curried = ([(ident', 0)], [EFunctionDef (TFun [] <$> erlangType) ss ident' [] (outerWrapper erl)])

      -- For effective > 0 (either plain curried funs, FnX or EffectFnX) generate an uncurried overload
      -- f x y = ... -~~> f(X,Y) -> ((...)(X))(Y).
      -- Relying on inlining to clean up some junk here
      let mkRunApp modName prefix n = App eann (Var eann (Qualified (P.ByModuleName modName) (Ident $ prefix <> T.pack (show n))))
          applyStep fn a = App eann fn (Var eann (Qualified P.ByNullSourcePos (Ident a)))

          countAbs :: Expr Ann -> Int
          countAbs (Abs _ _ e) = 1 + countAbs e
          countAbs _ = 0

      let curriedWrappingUncurried arity = do
            vars <- replicateM arity freshNameErl
            let app = EApp RegularApp (EAtomLiteral ident') (EVar <$> vars)
                callUncurriedErl = curriedLambda app vars
            pure
              ([(ident', 0)], [EFunctionDef (TFun [] <$> erlangType) ss ident' [] callUncurriedErl])
      let uncurriedWrappingCurried arity = do
            vars <- replicateM arity freshNameErl
            let callCurriedErl = curriedApp (EVar <$> vars) $ EApp RegularApp (EAtomLiteral ident') []
            pure
              ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars callCurriedErl])

      -- NOTE[drathier]: all call site arities are unknown here, since codegen runs as we go instead of as a separate
      -- second pass, so every arity is assumed to be used.
      -- Apply in CoreFn then translate to take advantage of translation of full/partial application
      (res1, res2) <- case effFnArity qident <|> fnArity qident <|> M.lookup qident arities of
        Just (EffFnXArity arity) -> do
          vars <- replicateM arity freshNameErl
          erl' <- valueToErl $ foldl applyStep (mkRunApp effectUncurried (snd C.P_runEffectFn) arity val) vars
          pure $ curried <> ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars (outerWrapper (EApp RegularApp erl' []))])
        Just (FnXArity arity) -> do
          -- Same as above
          vars <- replicateM arity freshNameErl
          erl' <- valueToErl $ foldl applyStep (mkRunApp dataFunctionUncurried (snd C.P_runFn) arity val) vars
          pure $ curried <> ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars (outerWrapper erl')])
        Just (Arity (n, m)) | n + m > 0 ->
          do
            let arity = n + m

            -- experimental split between typeclass & regular arguments
            -- TODO this still duplicates code
            vars <- replicateM arity freshNameErl
            split <-
              if n == 0 || m == 0
                then pure ([], [])
                else do
                  erl'' <- valueToErl $ foldl applyStep val (take n vars)
                  pure ([(ident', n)], [EFunctionDef Nothing ss ident' (take n vars) (outerWrapper erl'')])

            if countAbs val == arity
              then do
                erl' <- valueToErl $ foldl applyStep val vars
                curriedWrap <- curriedWrappingUncurried arity
                let uncurried = ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars (outerWrapper erl')])
                pure $ uncurried <> curriedWrap <> split
              else do
                uncurriedWrap <- uncurriedWrappingCurried arity
                pure $ curried <> uncurriedWrap <> split
        _ -> pure curried
      pure $
        if ident `Set.member` declaredExportsSet
          then (res1, res2, etypeEnv)
          else ([], res2, etypeEnv)

    bindToErl :: Bind Ann -> ErlW (Erl -> Erl)
    bindToErl bind =
      case bind of
        NonRec (ss,_,_) ident val -> do
          b <- EVarBind (identToVar ident) <$> valueToErl'' ss (Just ident) val
          pure (\innermost -> ELet b innermost)
          -- For recursive bindings F(X) = E1, G(X) = E2, ... we have a problem as the variables are not
          -- in scope until each expression is defined. To avoid lifting to the top level first generate
          -- funs which take a tuple of such funs F'({F', G'}) -> (X) -> E1 etc.
          -- with occurences of F, G replaced in E1, E2 with F'({F',G'})
          -- and then bind these F = F'({F',G'})
          -- TODO: Only do this if there are multiple mutually recursive bindings! Else a named fun works.
        Rec vals -> do
          let vars = identToVar . snd . fst <$> vals
              varTupInner = ETupleLiteral $ EVar . (<> "@fi") <$> vars
              varTupOuter = ETupleLiteral $ EVar . (<> "@f") <$> vars

              replaceFun fvar = everywhereOnErl go
                where
                  go (EVar f) | f == fvar = EApp RegularApp (EVar $ f <> "@fi") [varTupInner]
                  go e = e

          (funs :: [Erl]) <- forM vals $ \((_, ident), val) -> do
            erl <- valueToErl' Nothing val
            let erl' = foldr replaceFun erl vars
            let fun = EFunFull Nothing [(EFunBinder [varTupInner], erl')]
            pure $ EVarBind (identToVar ident <> "@f") fun
          let rebinds = map (\var -> EVarBind var (EApp RegularApp (EVar $ var <> "@f") [varTupOuter])) vars

          let s1 v = foldr EAndThen v funs
          let s2 v = foldr ELet v rebinds

          pure $ \innermost -> s1 (s2 innermost)


    qualifiedToVar (Qualified _ ident) = identToVar ident

    qualifiedToTypeclassCtor :: Qualified Ident -> Atom
    qualifiedToTypeclassCtor (Qualified (P.ByModuleName mn') ident)
      -- this case could be always qualified, but is not to assist optimisation
      | mn == mn' =
        Atom Nothing (runIdent' ident)
      | otherwise =
        Atom (Just $ atomModuleName mn' PureScriptModule) (runIdent' ident)
    qualifiedToTypeclassCtor (Qualified (P.BySourcePos  _) ident) = Atom Nothing (runIdent' ident)

    valueToErl :: Expr Ann -> ErlW Erl
    valueToErl e = valueToErl' Nothing e

    valueToErl' :: Maybe Ident -> Expr Ann -> ErlW Erl
    valueToErl' mIdent e =
      let (ss,_,_) =  extractAnn e in
      valueToErl'' ss mIdent e

    valueToErl2 :: SourceSpan -> Maybe Ident -> Expr Ann -> ErlW Erl
    valueToErl2 pss mIdent e =
      let
          (ss,_,_) = extractAnn e
          ss2 = case ss of
            SS.NullSourceSpan -> pss
            _ -> ss
      in
      valueToErl'' ss2 mIdent e

    valueToErl'' :: SourceSpan -> Maybe Ident -> Expr Ann -> ErlW Erl
    valueToErl'' ann _ (Literal (pos, _, _) l) =
      rethrowWithPosition pos $ literalToValueErl l
    valueToErl'' ann _ (Var _ (Qualified (P.ByModuleName C.M_Prim) (Ident undef)))
      | undef == C.S_undefined =
        return $ EAtomLiteral $ Atom Nothing C.S_undefined
    valueToErl'' ann _ (Var (_, _, Just (IsConstructor _ [])) (Qualified _ ident)) =
      return $ constructorLiteral (runIdent' ident) []
    valueToErl'' ann _ (Var _ ident) | isTopLevelBinding ident = pure $
      case M.lookup ident arities of
        Just (Arity (0, 1)) -> EFunRef (qualifiedToErl mn ident) 1
        _
          | Just (EffFnXArity arity) <- effFnArity ident,
            arity > 0 ->
            EFunRef (qualifiedToErl mn ident) arity
        _
          | Just (FnXArity arity) <- fnArity ident,
            arity > 0 ->
            EFunRef (qualifiedToErl mn ident) arity
        _ -> EApp RegularApp (EAtomLiteral $ qualifiedToErl mn ident) []
    valueToErl'' ann _ (Var _ ident) = return $ EVar $ qualifiedToVar ident
    valueToErl'' ann ident (Abs _ arg val) = do
      ret <- valueToErl2 ann Nothing val

      -- TODO this is mangled in corefn json
      let fixIdent (Ident "$__unused") = UnusedIdent
          fixIdent x = x
          arg' = case fixIdent arg of
            UnusedIdent -> "_"
            _ -> identToVar arg
      return $ EFun1 (fmap identToVar ident) arg' ret
    valueToErl'' ann _ (Accessor _ prop val) = do
      eval <- valueToErl2 ann Nothing val
      return $ EApp RegularApp (EAtomLiteral $ Atom (Just "maps") "get") [EAtomLiteral $ AtomPS Nothing prop, eval]
    valueToErl'' ann _ (ObjectUpdate _ o _mstring ps) = do
      obj <- valueToErl2 ann Nothing o
      sts <- mapM (sndM (valueToErl2 ann Nothing)) ps
      return $ EMapUpdate obj (map (first (AtomPS Nothing)) sts)
    valueToErl'' ann _ e@(App (_, _, meta) _ _) = do
      let (f, args) = unApp e []
          eMeta = case meta of
                          Just IsSyntheticApp -> SyntheticApp
                          _ -> RegularApp
      args' <- mapM (valueToErl2 ann Nothing) args
      case f of
        Var (_, _, Just IsNewtype) _ ->
          return $ head args'
        Var (_, _, Just (IsConstructor _ fields)) (Qualified _ ident)
          | length args == length fields ->
            return $ constructorLiteral (runIdent' ident) args'
        Var (_, _, Just IsTypeClassConstructor) name -> do
          let res = curriedApp args' $ EApp eMeta (EAtomLiteral $ qualifiedToTypeclassCtor name) []

              replaceQualifiedSelfCalls = \case
                (EAtomLiteral (Atom (Just emod) x)) | emod == atomModuleName mn PureScriptModule -> EAtomLiteral (Atom Nothing x)
                other -> other
          pure $ everywhereOnErl replaceQualifiedSelfCalls res

        -- fully saturated call to foreign import
        Var _ (Qualified _ ident)
          | isFullySaturatedForeignCall mn actualForeignArities f args ->
            return $ EApp eMeta (EAtomLiteral $ qualifiedToErl' mn ForeignModule ident) args'
        -- Fully saturated (consuming tc dicts and value args in 1 call) (including "over-saturated")
        Var _ qi@(Qualified _ _)
          | Just (Arity (n, m)) <- M.lookup qi arities,
            let arity = n + m,
            length args >= arity ->
            return $ curriedApp (drop arity args') $ EApp eMeta (EAtomLiteral (qualifiedToErl mn qi)) (take arity args')
        -- partially saturated application (all tc dicts to be applied in 1 call and maybe more to be applied to the curried result)
        Var _ qi@(Qualified _ _)
          | Just (Arity (n, _)) <- M.lookup qi arities,
            length args >= n,
            n > 0 ->
            return $ curriedApp (drop n args') $ EApp eMeta (EAtomLiteral (qualifiedToErl mn qi)) (take n args')
        _ -> curriedApp args' <$> valueToErl2 ann Nothing f
      where
        unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
        unApp (App _ val arg) args = unApp val (arg : args)
        unApp other args = (other, args)



    valueToErl'' ann _ (Case _ values branches) = do
      renamedValues <-
        forM values $ \v -> do
          v2 <- valueToErl2 ann Nothing v
          case v2 of
            EVar v2n -> pure (v2n, v2)
            _ -> (,) <$> freshNameErl' "CaseOf" <*> pure v2
      (res, resDB) <-
        runStateT
          (caseToErlImpl (map fst renamedValues) branches)
          (DB M.empty [] [] (map fst renamedValues))
      pure $
        letbindVars ELet renamedValues $
          letbind (\(k, v) rest -> ELet (EBind (EVar k) (EFun0 (Just k) v)) rest) (reverse (contImpls resDB)) res
      where
        caseToErlImpl :: [T.Text] -> [CaseAlternative Ann] -> StateT DB ErlW Erl
        caseToErlImpl values branches =
          ECaseOf (tupleWrap (map EVar values)) <$> branchesToErl branches

        tupleWrap [a] = a
        tupleWrap things = ETupleLiteral things

        -- Continuations are hashed and bound as local funs, so deeply nested branches don't duplicate code.
        buildCont :: [CaseAlternative Ann] -> StateT DB ErlW (Maybe T.Text)
        buildCont [] = pure Nothing
        buildCont branches = do
          db <- get
          case M.lookup branches (conts db) of
            Just contName -> pure (Just contName)
            Nothing -> do
              -- Insert the name before generating the branch, to avoid duplicate work.
              contName <- lift (freshNameErl' "Cont")
              put db {conts = M.insert branches contName (conts db)}
              rest <- caseToErlImpl (topmostValues db) branches
              modify (\db' -> db' {contImpls = (contName, rest) : contImpls db'})
              pure (Just contName)

        pushArrayCont :: T.Text -> [Binder Ann] -> StateT DB ErlW ()
        pushArrayCont var binders = modify (\db -> db {arrayConts = (var, binders) : arrayConts db})

        takeArrayConts :: StateT DB ErlW [(T.Text, [Binder Ann])]
        takeArrayConts = do
          db <- get
          put db {arrayConts = []}
          pure (arrayConts db)

        -- A catch-all branch calling the continuation for the remaining alternatives, if any.
        failureBranch :: Maybe T.Text -> [(EBinder, Erl)]
        failureBranch cont = [(EBinder (EVar "_"), EApp RegularApp (EVar c) []) | Just c <- [cont]]

        branchesToErl :: [CaseAlternative Ann] -> StateT DB ErlW [(EBinder, Erl)]
        branchesToErl [] = pure []
        branchesToErl (CaseAlternative binders mguard : restBranches) = do
          binders2 <- mapM binderToErl binders
          arrayConts <- takeArrayConts
          guarded <- case mguard of
            Right rhs -> (\rhs2 -> [(ETrue, rhs2)]) <$> lift (valueToErl2 ann Nothing rhs)
            Left guardedExprs ->
              forM guardedExprs (\(g, h) -> (,) <$> lift (valueToErl2 ann Nothing g) <*> lift (valueToErl2 ann Nothing h))
          case (mguard, guarded, arrayConts) of
            -- An unguarded alternative without array patterns can't fall through.
            (Right _, [(_, rhs)], []) -> ((EBinder (tupleWrap binders2), rhs) :) <$> branchesToErl restBranches
            _ -> do
              failureCont <- buildCont restBranches
              body <- arrayGuardsToErl failureCont arrayConts guarded
              pure $ (EBinder (tupleWrap binders2), body) : failureBranch failureCont

        arrayGuardsToErl :: Maybe T.Text -> [(T.Text, [Binder Ann])] -> [(Erl, Erl)] -> StateT DB ErlW Erl
        arrayGuardsToErl failureCont [] normalGuards = pure $ guardsToErl failureCont normalGuards
        arrayGuardsToErl failureCont ((arrayVar, binders) : restGuards) normalGuards = do
          binders2 <- mapM binderToErl binders
          arrayConts <- takeArrayConts
          restGuards2 <- arrayGuardsToErl failureCont (arrayConts <> restGuards) normalGuards
          let arrayCall f = EApp RegularApp (EAtomLiteral (AtomPS (Just "array") f)) [EVar arrayVar]
              contBranch = failureBranch failureCont
          pure $
            ECaseOf (arrayCall "size") $
              (EBinder (ENumericLiteral (Left (toInteger (length binders)))),
                ECaseOf (arrayCall "to_list") ((EBinder (EListLiteral binders2), restGuards2) : contBranch))
                : contBranch

        guardsToErl :: Maybe T.Text -> [(Erl, Erl)] -> Erl
        guardsToErl Nothing [] = EVar "Unreachable-drathier-unreachble-empty-guards-without-continuation"
        guardsToErl (Just failureCont) [] = EApp RegularApp (EVar failureCont) []
        guardsToErl failureCont ((guard, happy) : restGuards)
          | guard == ETrue = happy
          | otherwise =
              ECaseOf guard $
                (EBinder ETrue, happy)
                  : [(EBinder EFalse, guardsToErl failureCont restGuards) | not (null restGuards && failureCont == Nothing)]

        binderToErl :: Binder Ann -> StateT DB ErlW Erl
        binderToErl = \case
          NullBinder _ -> pure $ EVar "_"
          VarBinder _ name -> pure $ EVar (identToVar name)
          ConstructorBinder (_, _, Just IsNewtype) _ _ [binder] -> binderToErl binder
          ConstructorBinder _ _typeName (Qualified _ (ProperName ctorName)) binders ->
            constructorLiteral ctorName <$> mapM binderToErl binders
          NamedBinder _ alias binder -> EBind (EVar (identToVar alias)) <$> binderToErl binder
          LiteralBinder _ lit ->
            case lit of
              NumericLiteral n -> pure $ ENumericLiteral n
              StringLiteral psString -> pure $ EStringLiteral psString
              CharLiteral char -> pure $ ECharLiteral char
              BooleanLiteral True -> pure ETrue
              BooleanLiteral False -> pure EFalse
              ObjectLiteral kvPairs ->
                EMapPattern <$> mapM (\(k, v) -> (AtomPS Nothing k,) <$> binderToErl v) kvPairs
              ArrayLiteral items -> do
                arrayVar <- freshNameErl' "ArrayLiteral"
                pushArrayCont arrayVar items
                pure (EVar arrayVar)

    valueToErl'' ann _ (Let _ ds val) = do
      ds2 <- mapM bindToErl ds
      ret <- valueToErl2 ann Nothing val
      let ds3 = foldr ($) ret ds2
      pure ds3

    valueToErl'' ann _ (Constructor (_, _, Just IsNewtype) _ _ _) = error "newtype ctor"
    valueToErl'' ann _ (Constructor _ _ (ProperName ctor) fields) =
      let createFn =
            let body = constructorLiteral ctor ((EVar . identToVar) `map` fields)
             in foldr (EFun1 Nothing . identToVar) body fields
       in pure createFn

    constructorLiteral name args = ETupleLiteral (EAtomLiteral (Atom Nothing (toAtomName name)) : args)

    literalToValueErl :: Literal (Expr Ann) -> ErlW Erl
    literalToValueErl = \case
      NumericLiteral n -> pure $ ENumericLiteral n
      StringLiteral s -> pure $ EStringLiteral s
      CharLiteral c -> pure $ ECharLiteral c
      BooleanLiteral b -> pure $ if b then ETrue else EFalse
      ArrayLiteral xs -> EArrayLiteral <$> mapM valueToErl xs
      ObjectLiteral ps -> EMapLiteral . map (first (AtomPS Nothing)) <$> mapM (sndM valueToErl) ps

    ensureFreshVars_ = ensureFreshVars Set.empty M.empty

    ensureFreshVars :: Set Ident -> Map Ident Ident -> Expr Ann -> ErlW (Expr Ann)
    ensureFreshVars scope vars = go
      where
        go :: Expr Ann -> ErlW (Expr Ann)
        go expr = case expr of
          Literal ann lit ->
            Literal ann <$> case lit of
              ArrayLiteral exs -> ArrayLiteral <$> traverse go exs
              ObjectLiteral fields -> ObjectLiteral <$> traverse (traverse go) fields
              _ -> pure lit
          Constructor {} -> pure expr
          Accessor ann ps ex -> Accessor ann ps <$> go ex
          ObjectUpdate ann e mstring updates -> ObjectUpdate ann <$> go e <*> pure mstring <*> traverse (traverse go) updates
          Abs ann ident e -> do
            (ident', scope', vars') <- bindIdent scope vars ident
            Abs ann ident' <$> ensureFreshVars scope' vars' e
          App ann e1 e2 -> App ann <$> go e1 <*> go e2
          (Var a (Qualified q@(P.BySourcePos  _) x)) | x `notElem` topLevelNames -> pure $ Var a $ Qualified q $ fromMaybe x $ M.lookup x vars
          otherVar@Var {} -> pure otherVar
          Case ann es cases -> Case ann <$> traverse go es <*> traverse goCase cases
          Let ann binds e -> do
            (scope', vars', binds') <- foldM goBind (scope, vars, []) binds
            Let ann (reverse binds') <$> ensureFreshVars scope' vars' e

        bindIdent scope vars = \case
          ident@UnusedIdent -> pure (ident, scope, vars)
          Ident "$__unused" -> pure (UnusedIdent, scope, vars)
          ident@P.GenIdent {} -> pure (ident, scope, vars)
          ident@(Ident rawIdent) ->
            if lowerIdent ident `Set.member` scope
              then do
                newIdent <- Ident <$> freshNameErl' rawIdent
                pure (newIdent, Set.insert (lowerIdent newIdent) scope, M.insert ident newIdent vars)
              else pure (ident, Set.insert (lowerIdent ident) scope, vars)
          where
            lowerIdent (Ident x) = Ident $ T.toLower x
            lowerIdent other = other

        goBind :: (Set Ident, Map Ident Ident, [Bind Ann]) -> Bind Ann -> ErlW (Set Ident, Map Ident Ident, [Bind Ann])
        goBind (scope, vars, acc) = \case
          NonRec ann ident e -> do
            (ident', scope', vars') <- bindIdent scope vars ident
            e' <- ensureFreshVars scope' vars' e
            pure (scope', vars', NonRec ann ident' e' : acc)
          Rec binds -> do
            let idents = snd . fst <$> binds
            (idents', scope', vars') <-
              foldM
                ( \(accIdents, accScope, accVars) ident -> do
                    (ident', scope', vars') <- bindIdent accScope accVars ident
                    pure (ident' : accIdents, scope', vars')
                )
                ([], scope, vars)
                idents
            let f (newIdent, ((ann, _oldIdent), e)) = ((ann, newIdent),) <$> ensureFreshVars scope' vars' e
            binds' <- traverse f $ zip (reverse idents') binds
            pure (scope', vars', Rec binds' : acc)

        goCase (CaseAlternative ann (Right e)) =
          CaseAlternative ann . Right <$> go e
        goCase (CaseAlternative ann (Left ges)) =
          CaseAlternative ann . Left
            <$> traverse (traverse go) ges

letbindVars elet exprs innermost =
  case exprs of
    [] -> innermost
    (name,e):es ->
      case EVar name == e of
        True -> letbindVars elet es innermost
        False -> elet (EBind (EVar name) e) (letbindVars elet es innermost)

letbind elet exprs innermost =
  case exprs of
    [] -> innermost
    e:es -> elet e (letbind elet es innermost)



-- NOTE[drathier]: hash continuations and bind them as local funs, so we don't duplicate code on deeply nested branches
data DB
  = DB
    { conts :: M.Map [CaseAlternative Ann] T.Text
    , contImpls :: [(T.Text, Erl)]
    , arrayConts :: [(T.Text, [Binder Ann])]
    , topmostValues :: [T.Text]
    }
