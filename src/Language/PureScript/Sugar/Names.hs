module Language.PureScript.Sugar.Names
  ( desugarImports
  , Env
  , externsEnv
  , primEnv
  , ImportRecord(..)
  , ImportProvenance(..)
  , Imports(..)
  , Exports(..)
  ) where

import Prelude
import Protolude (sortOn, swap, foldl')

import Control.Arrow (first, second, (&&&))
import Control.Monad (when, (>=>))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Strict (MonadState, StateT(..), gets, modify)

import Data.List.NonEmpty qualified as NEL
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Map.Strict qualified as M
import Data.Set qualified as S

import Language.PureScript.AST
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), addHint, errorMessage, errorMessage'', nonEmpty, parU, warnAndRethrow, warnAndRethrowWithPosition)
import Language.PureScript.Externs (ExternsDeclaration(..), ExternsFile(..))
import Language.PureScript.Linter.Imports (Name(..), UsedImports)
import Language.PureScript.Names (pattern ByNullSourcePos, Ident, OpName, OpNameType(..), ProperName, ProperNameType(..), Qualified(..), QualifiedBy(..))
import Language.PureScript.Sugar.Names.Env (Env, Exports(..), ImportProvenance(..), ImportRecord(..), Imports(..), checkImportConflicts, nullImports, primEnv)
import Language.PureScript.Sugar.Names.Exports (findExportable, resolveExports)
import Language.PureScript.Sugar.Names.Imports (resolveImports)
import Language.PureScript.Sugar.Names.Resolve (ResolveM, runResolveM)
import Language.PureScript.Sugar.Monad (DesugarM)
import Language.PureScript.Traversals (defS, sndM)
import Language.PureScript.Types (Constraint(..), SourceConstraint, SourceType, Type(..), everywhereOnTypesM)

-- |
-- Replaces all local names with qualified names.
--
desugarImports
  :: Module
  -> DesugarM Module
desugarImports = updateEnv >=> renameInModule'
  where
  updateEnv :: Module -> DesugarM Module
  updateEnv m@(Module ss _ mn _ refs) = do
    members <- runResolveM $ findExportable m
    env' <- gets $ M.insert mn (ss, nullImports, members) . fst
    (m', imps) <- runResolveM $ resolveImports env' m
    exps <- maybe (return members) (runResolveM . resolveExports env' ss mn imps members) refs
    modify . first $ M.insert mn (ss, imps, exps)
    return m'

  renameInModule' :: Module -> DesugarM Module
  renameInModule' m@(Module _ _ mn _ _) =
    warnAndRethrow (addHint (ErrorInModule mn)) $ do
      env <- gets fst
      let (_, imps, exps) = fromMaybe (internalError "Module is missing in renameInModule'") $ M.lookup mn env
      (m', used) <- flip runStateT M.empty $ renameInModule imps m
      modify . second $ M.unionWith (<>) used
      return $ elaborateExports exps m'

-- | Create an environment from a collection of externs files
externsEnv
  :: Env
  -> ExternsFile
  -> ResolveM Env
externsEnv env ExternsFile{..} =
  -- `efExports` is already fully elaborated -- re-exported names are baked
  -- in as `ReExportRef`s carrying their own `ExportSource` provenance -- so
  -- `members` is already the complete, correct answer (local declarations
  -- and re-exports alike) without needing to resolve anything against this
  -- or any other module's `env` entry. This is what makes folding externs
  -- files into `Env` order-independent: unlike `updateEnv` (which processes
  -- a freshly-parsed module's raw, unelaborated export list and genuinely
  -- needs the full import/re-export resolution machinery), a reloaded
  -- ExternsFile has nothing left to resolve.
  pure $ M.insert efModuleName (efSourceSpan, nullImports, members) env
  where

  members = Exports{..}

  -- An ExportSource for declarations local to the module which the given
  -- ExternsFile corresponds to.
  localExportSource =
    ExportSource { exportSourceDefinedIn = efModuleName
                  , exportSourceImportedFrom = Nothing
                  }

  exportedTypes :: M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ExportSource)
  exportedTypes = M.fromList $ mapMaybe toExportedType efExports
    where
    toExportedType (TypeRef _ tyCon dctors) = Just (tyCon, (fromMaybe (mapMaybe forTyCon efDeclarations) dctors, localExportSource))
      where
      forTyCon :: ExternsDeclaration -> Maybe (ProperName 'ConstructorName)
      forTyCon (EDDataConstructor pn _ tNm _ _) | tNm == tyCon = Just pn
      forTyCon _ = Nothing
    toExportedType (ReExportRef _ src (TypeRef _ tyCon dctors)) = Just (tyCon, (fromMaybe [] dctors, src))
    toExportedType _ = Nothing

  exportedTypeOps :: M.Map (OpName 'TypeOpName) ExportSource
  exportedTypeOps = exportedRefs getTypeOpRef

  exportedTypeClasses :: M.Map (ProperName 'ClassName) ExportSource
  exportedTypeClasses = exportedRefs getTypeClassRef

  exportedValues :: M.Map Ident ExportSource
  exportedValues = exportedRefs getValueRef

  exportedValueOps :: M.Map (OpName 'ValueOpName) ExportSource
  exportedValueOps = exportedRefs getValueOpRef

  -- Re-exported refs are wrapped in `ReExportRef`, carrying the original
  -- module's `ExportSource` directly; everything else was declared locally.
  exportedRefs :: Ord a => (DeclarationRef -> Maybe a) -> M.Map a ExportSource
  exportedRefs f =
    M.fromList $ mapMaybe extract efExports
    where
    extract (ReExportRef _ src ref) = (, src) <$> f ref
    extract ref = (, localExportSource) <$> f ref

-- |
-- Make all exports for a module explicit. This may still affect modules that
-- have an exports list, as it will also make all data constructor exports
-- explicit.
--
-- The exports will appear in the same order as they do in the existing exports
-- list, or if there is no export list, declarations are order based on their
-- order of appearance in the module.
--
elaborateExports :: Exports -> Module -> Module
elaborateExports exps (Module ss coms mn decls refs) =
  Module ss coms mn decls $ Just $ reorderExports decls refs
    $ elaboratedTypeRefs
    ++ go (TypeOpRef ss) exportedTypeOps
    ++ go (TypeClassRef ss) exportedTypeClasses
    ++ go (ValueRef ss) exportedValues
    ++ go (ValueOpRef ss) exportedValueOps
    ++ maybe [] (filter isModuleRef) refs
  where

  elaboratedTypeRefs :: [DeclarationRef]
  elaboratedTypeRefs =
    flip map (M.toList (exportedTypes exps)) $ \(tctor, (dctors, src)) ->
      let ref = TypeRef ss tctor (Just dctors)
      in if mn == exportSourceDefinedIn src then ref else ReExportRef ss src ref

  go :: (a -> DeclarationRef) -> (Exports -> M.Map a ExportSource) -> [DeclarationRef]
  go toRef select =
    flip map (M.toList (select exps)) $ \(export, src) ->
      if mn == exportSourceDefinedIn src then toRef export else ReExportRef ss src (toRef export)

-- |
-- Given a list of declarations, an original exports list, and an elaborated
-- exports list, reorder the elaborated list so that it matches the original
-- order. If there is no original exports list, reorder declarations based on
-- their order in the source file.
reorderExports :: [Declaration] -> Maybe [DeclarationRef] -> [DeclarationRef] -> [DeclarationRef]
reorderExports decls originalRefs =
  sortOn originalIndex
  where
  names =
    maybe (mapMaybe declName decls) (map declRefName) originalRefs
  namesMap =
    M.fromList $ zip names [(0::Int)..]
  originalIndex ref =
    M.lookup (declRefName ref) namesMap

type Rename = StateT UsedImports DesugarM
type Bound = M.Map Ident SourcePos
-- Current source span and the locally bound identifiers.
type Ctx = (SourceSpan, Bound)

-- |
-- Replaces all local names with qualified names within a module and checks that all existing
-- qualified names are valid.
--
renameInModule
  :: Imports
  -> Module
  -> Rename Module
renameInModule imports (Module modSS coms mn decls exps) =
  Module modSS coms mn <$> parU decls go <*> pure exps
  where

  (go, _, _, _, _, _) =
    everywhereWithContextOnValuesM
      (modSS, M.empty)
      (\(_, bound) d -> (\(bound', d') -> ((declSourceSpan d', bound'), d')) <$> updateDecl bound d)
      updateValue
      updateBinder
      updateCase
      defS
      updateGuard

  -- Only extern declarations introduce a bound name at this level.
  updateDecl :: Bound -> Declaration -> Rename (Bound, Declaration)
  updateDecl bound d = (bound', ) <$> renameDecl d
    where
    bound' = case d of
      ExternDeclaration sa name _ -> M.insert name (spanStart $ safst sa) bound
      _ -> bound

  renameDecl :: Declaration -> Rename Declaration
  renameDecl = \case
    DataDeclaration sa dtype name args dctors ->
      DataDeclaration sa dtype name
        <$> updateTypeArguments args
        <*> traverse (traverseDataCtorFields (traverse (sndM updateTypesEverywhere))) dctors
    TypeSynonymDeclaration sa name ps ty ->
      TypeSynonymDeclaration sa name
        <$> updateTypeArguments ps
        <*> updateTypesEverywhere ty
    TypeClassDeclaration sa className args implies deps ds ->
      TypeClassDeclaration sa className
        <$> updateTypeArguments args
        <*> updateConstraints implies
        <*> pure deps
        <*> pure ds
    TypeInstanceDeclaration sa na@(SourceAnn ss _) ch idx name cs cn ts ds ->
      TypeInstanceDeclaration sa na ch idx name
        <$> updateConstraints cs
        <*> updateClassName cn ss
        <*> traverse updateTypesEverywhere ts
        <*> pure ds
    KindDeclaration sa kindFor name ty ->
      KindDeclaration sa kindFor name <$> updateTypesEverywhere ty
    TypeDeclaration (TypeDeclarationData sa name ty) ->
      TypeDeclaration . TypeDeclarationData sa name <$> updateTypesEverywhere ty
    ExternDeclaration sa name ty ->
      ExternDeclaration sa name <$> updateTypesEverywhere ty
    ExternDataDeclaration sa name ki ->
      ExternDataDeclaration sa name <$> updateTypesEverywhere ki
    TypeFixityDeclaration sa@(SourceAnn ss _) fixity alias op ->
      TypeFixityDeclaration sa fixity
        <$> updateTypeName alias ss
        <*> pure op
    ValueFixityDeclaration sa@(SourceAnn ss _) fixity (Qualified mn' (Left alias)) op ->
      ValueFixityDeclaration sa fixity . fmap Left
        <$> updateValueName (Qualified mn' alias) ss
        <*> pure op
    ValueFixityDeclaration sa@(SourceAnn ss _) fixity (Qualified mn' (Right alias)) op ->
      ValueFixityDeclaration sa fixity . fmap Right
        <$> updateDataConstructorName (Qualified mn' alias) ss
        <*> pure op
    d -> pure d

  updateValue
    :: Ctx
    -> Expr
    -> Rename (Ctx, Expr)
  updateValue (_, bound) v@(PositionedValue pos' _ _) =
    return ((pos', bound), v)
  updateValue (pos, bound) (Abs (VarBinder ss arg) val') =
    return ((pos, M.insert arg (spanStart ss) bound), Abs (VarBinder ss arg) val')
  updateValue (pos, bound) (Let w ds val') = do
    let
      args = mapMaybe letBoundVariable ds
      groupByFst = map (\ts -> (fst (NEL.head ts), snd <$> ts)) . NEL.groupAllWith fst
      duplicateArgsErrs = foldMap mkArgError $ groupByFst args
      mkArgError (ident, poses)
        | NEL.length poses < 2 = mempty
        | otherwise = errorMessage'' (NEL.reverse poses) (OverlappingNamesInLet ident)
    when (nonEmpty duplicateArgsErrs) $
      throwError duplicateArgsErrs
    return ((pos, declarationsToMap ds `M.union` bound), Let w ds val')
  updateValue (_, bound) (Var ss name'@(Qualified qualifiedBy ident)) =
    ((ss, bound), ) <$> case (M.lookup ident bound, qualifiedBy) of
      -- bound idents that have yet to be locally qualified.
      (Just sourcePos, ByNullSourcePos) ->
        pure $ Var ss (Qualified (BySourcePos sourcePos) ident)
      -- unbound idents are likely import unqualified imports, so we
      -- handle them through updateValueName if they don't exist as a
      -- local binding.
      (Nothing, ByNullSourcePos) ->
        Var ss <$> updateValueName name' ss
      -- bound/unbound idents with explicit qualification is still
      -- handled through updateValueName, as it fully resolves the
      -- ModuleName.
      (_, ByModuleName _) ->
        Var ss <$> updateValueName name' ss
      -- encountering non-null source spans may be a bug in previous
      -- desugaring steps or with the AST traversals.
      (_, BySourcePos _) ->
        internalError "updateValue: ident is locally-qualified by a non-null source position"
  updateValue (_, bound) (Op ss op) =
    ((ss, bound), ) <$> (Op ss <$> updateValueOpName op ss)
  updateValue (_, bound) (Constructor ss name) =
    ((ss, bound), ) <$> (Constructor ss <$> updateDataConstructorName name ss)
  updateValue s (TypedValue check val ty) =
    (s, ) <$> (TypedValue check val <$> updateTypesEverywhere ty)
  updateValue s (VisibleTypeApp val ty) =
    (s, ) <$> VisibleTypeApp val <$> updateTypesEverywhere ty
  updateValue s v = return (s, v)

  updateBinder
    :: Ctx
    -> Binder
    -> Rename (Ctx, Binder)
  updateBinder (_, bound) v@(PositionedBinder pos _ _) =
    return ((pos, bound), v)
  updateBinder (_, bound) (ConstructorBinder ss name b) =
    ((ss, bound), ) <$> (ConstructorBinder ss <$> updateDataConstructorName name ss <*> pure b)
  updateBinder (_, bound) (OpBinder ss op) =
    ((ss, bound), ) <$> (OpBinder ss <$> updateValueOpName op ss)
  updateBinder s (TypedBinder t b) = do
    t' <- updateTypesEverywhere t
    return (s, TypedBinder t' b)
  updateBinder s v =
    return (s, v)

  updateCase
    :: Ctx
    -> CaseAlternative
    -> Rename (Ctx, CaseAlternative)
  updateCase (pos, bound) c@(CaseAlternative bs _) =
    return ((pos, rUnionMap binderNamesWithSpans' bs `M.union` bound), c)
    where
    rUnionMap f = foldl' (flip (M.union . f)) M.empty

  updateGuard
    :: Ctx
    -> Guard
    -> Rename (Ctx, Guard)
  updateGuard (pos, bound) g@(ConditionGuard _) =
    return ((pos, bound), g)
  updateGuard (pos, bound) g@(PatternGuard b _) =
    return ((pos, binderNamesWithSpans' b `M.union` bound), g)

  binderNamesWithSpans' :: Binder -> Bound
  binderNamesWithSpans'
    = M.fromList
    . fmap (second spanStart . swap)
    . binderNamesWithSpans

  letBoundVariable :: Declaration -> Maybe (Ident, SourceSpan)
  letBoundVariable = fmap (valdeclIdent &&& (safst . valdeclSourceAnn)) . getValueDeclaration

  declarationsToMap :: [Declaration] -> Bound
  declarationsToMap = foldl goDTM M.empty
    where
      goDTM a (ValueDeclaration ValueDeclarationData {..}) =
        M.insert valdeclIdent (spanStart $ safst valdeclSourceAnn) a
      goDTM a _ =
        a

  updateTypeArguments
    :: (Traversable f, Traversable g)
    => f (a, g SourceType) -> Rename (f (a, g SourceType))
  updateTypeArguments = traverse (sndM (traverse updateTypesEverywhere))

  updateTypesEverywhere :: SourceType -> Rename SourceType
  updateTypesEverywhere = everywhereOnTypesM updateType
    where
    updateType :: SourceType -> Rename SourceType
    updateType (TypeOp ann@(SourceAnn ss _) name) = TypeOp ann <$> updateTypeOpName name ss
    updateType (TypeConstructor ann@(SourceAnn ss _) name) = TypeConstructor ann <$> updateTypeName name ss
    updateType (ConstrainedType ann c t) = ConstrainedType ann <$> updateInConstraint c <*> pure t
    updateType t = return t
    updateInConstraint :: SourceConstraint -> Rename SourceConstraint
    updateInConstraint (Constraint ann@(SourceAnn ss _) name ks ts info) =
      Constraint ann <$> updateClassName name ss <*> pure ks <*> pure ts <*> pure info

  updateConstraints :: [SourceConstraint] -> Rename [SourceConstraint]
  updateConstraints = traverse $ \(Constraint ann@(SourceAnn pos _) name ks ts info) ->
    Constraint ann
      <$> updateClassName name pos
      <*> traverse updateTypesEverywhere ks
      <*> traverse updateTypesEverywhere ts
      <*> pure info

  updateTypeName
    :: Qualified (ProperName 'TypeName)
    -> SourceSpan
    -> Rename (Qualified (ProperName 'TypeName))
  updateTypeName = update (importedTypes imports) TyName

  updateTypeOpName
    :: Qualified (OpName 'TypeOpName)
    -> SourceSpan
    -> Rename (Qualified (OpName 'TypeOpName))
  updateTypeOpName = update (importedTypeOps imports) TyOpName

  updateDataConstructorName
    :: Qualified (ProperName 'ConstructorName)
    -> SourceSpan
    -> Rename (Qualified (ProperName 'ConstructorName))
  updateDataConstructorName = update (importedDataConstructors imports) DctorName

  updateClassName
    :: Qualified (ProperName 'ClassName)
    -> SourceSpan
    -> Rename (Qualified (ProperName 'ClassName))
  updateClassName = update (importedTypeClasses imports) TyClassName

  updateValueName :: Qualified Ident -> SourceSpan -> Rename (Qualified Ident)
  updateValueName = update (importedValues imports) IdentName

  updateValueOpName
    :: Qualified (OpName 'ValueOpName)
    -> SourceSpan
    -> Rename (Qualified (OpName 'ValueOpName))
  updateValueOpName = update (importedValueOps imports) ValOpName

  -- Update names so unqualified references become qualified, and locally
  -- qualified references are replaced with their canonical qualified names
  -- (e.g. M.Map -> Data.Map.Map).
  update
    :: (Ord a)
    => M.Map (Qualified a) [ImportRecord a]
    -> (a -> Name)
    -> Qualified a
    -> SourceSpan
    -> Rename (Qualified a)
  update imps toName qname@(Qualified mn' name) pos = warnAndRethrowWithPosition pos $
    case (M.lookup qname imps, mn') of

      -- We found the name in our imports, so we return the name for it,
      -- qualifying with the name of the module it was originally defined in
      -- rather than the module we're importing from, to handle the case of
      -- re-exports. If there are multiple options for the name to resolve to
      -- in scope, we throw an error.
      (Just options, _) -> do
        (mnNew, mnOrig) <- runResolveM $ checkImportConflicts pos mn toName options
        modify $ \usedImports ->
          M.insertWith (++) mnNew [fmap toName qname] usedImports
        return $ Qualified (ByModuleName mnOrig) name

      -- If the name wasn't found in our imports but was qualified then we need
      -- to check whether it's a failed import from a "pseudo" module (created
      -- by qualified importing). If that's not the case, then we just need to
      -- check it refers to a symbol in another module.
      (Nothing, ByModuleName mn'') ->
        if mn'' `S.member` importedQualModules imports || mn'' `S.member` importedModules imports
        then throwUnknown
        else throwError . errorMessage . UnknownName . Qualified ByNullSourcePos $ ModName mn''

      -- If neither of the above cases are true then it's an undefined or
      -- unimported symbol.
      _ -> throwUnknown

    where
    throwUnknown = throwError . errorMessage . UnknownName . fmap toName $ qname
