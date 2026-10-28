-- |
-- This module implements the desugaring pass which reapplies binary operators based
-- on their fixity data and removes explicit parentheses.
--
-- The value parser ignores fixity data when parsing binary operator applications, so
-- it is necessary to reorder them here.
--
module Language.PureScript.Sugar.Operators
  ( desugarSignedLiterals
  , RebracketCaller(..)
  , rebracket
  , rebracketFiltered
  , checkFixityExports
  ) where

import Prelude

import Language.PureScript.AST
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), addHint, errorMessage, errorMessage', parU, rethrow, rethrowWithPosition)
import Language.PureScript.Externs (ExternsFile(..), ExternsFixity(..), ExternsTypeFixity(..))
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), Name(..), OpName, OpNameType(..), ProperName, ProperNameType(..), Qualified(..), QualifiedBy(..), freshIdent')
import Language.PureScript.Sugar.Operators.Binders (matchBinderOperators)
import Language.PureScript.Sugar.Operators.Expr (matchExprOperators)
import Language.PureScript.Sugar.Operators.Types (matchTypeOperators)
import Language.PureScript.Sugar.Monad (DesugarM)
import Language.PureScript.Traversals (defS, sndM)
import Language.PureScript.Types (Constraint(..), SourceType, Type(..), everythingOnTypes, everywhereOnTypesTopDownM, overConstraintArgs)

import Control.Monad (unless, (<=<))
import Control.Monad.Error.Class (throwError)

import Data.Bits ((.&.), (.|.))
import Data.Either (partitionEithers)
import Data.Foldable (fold, foldl', for_, traverse_)
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.List (groupBy, sortOn)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Map.Strict qualified as M
import Data.Ord (Down(..))

import Language.PureScript.Constants.Libs qualified as C

-- |
-- Removes unary negation operators and replaces them with calls to `negate`.
--
desugarSignedLiterals :: Module -> Module
desugarSignedLiterals (Module ss coms mn ds exts) =
  Module ss coms mn (map f' ds) exts
  where
  (f', _, _) = everywhereOnValues id go id
  go (UnaryMinus ss' val) = App (Var ss' (Qualified ByNullSourcePos (Ident C.S_negate))) val
  go other = other

-- |
-- An operator associated with its declaration position, fixity, and the name
-- of the function or data constructor it is an alias for.
--
type FixityRecord op alias = (Qualified op, SourceSpan, Fixity, Qualified alias)
type ValueFixityRecord = FixityRecord (OpName 'ValueOpName) (Either Ident (ProperName 'ConstructorName))
type TypeFixityRecord = FixityRecord (OpName 'TypeOpName) (ProperName 'TypeName)

-- |
-- Remove explicit parentheses and reorder binary operator applications.
--
-- This pass requires name desugaring and export elaboration to have run first.
--
rebracket
  :: [ExternsFile]
  -> Module
  -> DesugarM Module
rebracket =
  rebracketFiltered CalledByCompile (const True)

-- |
-- A version of `rebracket` which allows you to choose which declarations
-- should be affected. This is used in docs generation, where we want to
-- desugar type operators in instance declarations to ensure that instances are
-- paired up with their types correctly, but we don't want to desugar type
-- operators in value declarations.
--
rebracketFiltered
  :: RebracketCaller
  -> (Declaration -> Bool)
  -> [ExternsFile]
  -> Module
  -> DesugarM Module
rebracketFiltered !caller pred_ externs m = do
  let (valueFixities, typeFixities) =
        partitionEithers
          $ concatMap externsFixities externs
          ++ collectFixities m

  ensureNoDuplicates' MultipleValueOpFixities valueFixities
  ensureNoDuplicates' MultipleTypeOpFixities typeFixities

  let valueOpTable = customOperatorTable' valueFixities
  let valueAliased = M.fromList (map makeLookupEntry valueFixities)
  let typeOpTable = customOperatorTable' typeFixities
  let typeAliased = M.fromList (map makeLookupEntry typeFixities)

  rebracketModule caller pred_ valueOpTable typeOpTable valueAliased typeAliased m

  where

  ensureNoDuplicates'
    :: Ord op
    => (op -> SimpleErrorMessage)
    -> [FixityRecord op alias]
    -> DesugarM ()
  ensureNoDuplicates' toError =
    ensureNoDuplicates toError . map (\(i, pos, _, _) -> (i, pos))

  customOperatorTable'
    :: [FixityRecord op alias]
    -> [[(Qualified op, Associativity)]]
  customOperatorTable' = customOperatorTable . map (\(i, _, f, _) -> (i, f))

  makeLookupEntry :: FixityRecord op alias -> (Qualified op, Qualified alias)
  makeLookupEntry (qname, _, _, alias) = (qname, alias)

-- | Indicates whether the `rebracketModule`
-- is being called with the full desugar pass
-- run via `purs compile` or whether
-- only the partial desugar pass is run
-- via `purs docs`.
-- This indication is needed to prevent
-- a `purs docs` error when using
-- `case _ of` syntax in a type class instance.
data RebracketCaller
  = CalledByCompile
  | CalledByDocs
  deriving (Eq, Show)

type ValueAliases = M.Map (Qualified (OpName 'ValueOpName)) (Qualified (Either Ident (ProperName 'ConstructorName)))
type TypeAliases = M.Map (Qualified (OpName 'TypeOpName)) (Qualified (ProperName 'TypeName))

rebracketModule
  :: RebracketCaller
  -> (Declaration -> Bool)
  -> [[(Qualified (OpName 'ValueOpName), Associativity)]]
  -> [[(Qualified (OpName 'TypeOpName), Associativity)]]
  -> ValueAliases
  -> TypeAliases
  -> Module
  -> DesugarM Module
rebracketModule !caller pred_ valueOpTable typeOpTable valueAliased typeAliased (Module ss coms mn ds exts) =
  Module ss coms mn <$> (stageB =<< stageA) <*> pure exts
  where
  needed = map needs ds

  -- Reordering can only fail, so it runs in 'Either'; the errors of all the
  -- declarations are collected like 'parU' does.
  stageA =
    case partitionEithers [if pred_ d && n .&. needsReorder /= 0 then f d else Right d | (d, n) <- zip ds needed] of
      ([], ds') -> pure ds'
      (errs, _) -> throwError (fold errs)

  stageB ds' = parU (zip ds' needed) $ \(d, n) ->
    if pred_ d && n .&. needsFinish /= 0 then finish d else pure d

  -- The AST will run through all the desugar passes when compiling
  -- and only some of the desugar passes when generating docs.
  -- When generating docs, `case _ of` syntax used in an instance declaration
  -- can trigger the `IncorrectAnonymousArgument` error because it does not
  -- run the same passes that the compile desugaring does. Since `purs docs`
  -- will only succeed once `purs compile` succeeds, we can ignore this check
  -- when running `purs docs`.
  -- See https://github.com/purescript/purescript/issues/4274#issuecomment-1087730651=
  -- for more info.
  compiling = caller == CalledByCompile

  -- Reorders the operator chains. This has to see the explicit parentheses,
  -- which delimit the chains.
  (f, _, _, _, _, _) =
      everywhereWithContextOnValuesM
        ss
        (\_ d -> (declSourceSpan d,) <$> goDecl d)
        (\pos -> wrap (matchExprOperators valueOpTable) <=< goExpr' pos)
        (\pos -> wrap (matchBinderOperators valueOpTable) <=< goBinder' pos)
        defS
        defS
        defS

  (goDecl, goExpr', goBinder') = updateTypes (flip matchTypeOperators typeOpTable)

  wrap :: (a -> Either MultipleErrors a) -> (SourceSpan, a) -> Either MultipleErrors (SourceSpan, a)
  wrap go (ss', a) = (ss',) <$> go a

  -- Everything that is left to do once the chains are reordered, in a single
  -- traversal: turn the remaining binary applications and sections into
  -- applications and lambdas, remove the explicit parentheses and replace the
  -- operators with the functions and constructors they alias.
  (finish, _, _, _, _, _) =
    everywhereWithContextOnValuesM
      ss
      (\_ d -> (declSourceSpan d,) <$> goDecl2 d)
      goExprFull
      goBinderFull
      defS
      defS
      defS

  (goDecl2, goExpr2', goBinder2') = updateTypes goType2

  goExprFull :: SourceSpan -> Expr -> DesugarM (SourceSpan, Expr)
  goExprFull pos = uncurry goExpr2 <=< goExpr2' pos

  goExpr2 :: SourceSpan -> Expr -> DesugarM (SourceSpan, Expr)
  goExpr2 pos e0 = do
    e <- if compiling then removeBinaryNoParens e0 else pure e0
    case e of
      Parens val -> goExprFull pos val
      PositionedValue pos' _ _ -> return (pos', e)
      Op pos' op ->
        (pos',) <$> case op `M.lookup` valueAliased of
          Just (Qualified mn' (Left alias)) ->
            return $ Var pos' (Qualified mn' alias)
          Just (Qualified mn' (Right alias)) ->
            return $ Constructor pos' (Qualified mn' alias)
          Nothing ->
            throwError . errorMessage' pos' . UnknownName $ fmap ValOpName op
      _ -> return (pos, e)

  goBinderFull :: SourceSpan -> Binder -> DesugarM (SourceSpan, Binder)
  goBinderFull pos = uncurry goBinder2 <=< goBinder2' pos

  goBinder2 :: SourceSpan -> Binder -> DesugarM (SourceSpan, Binder)
  goBinder2 pos = \case
    ParensInBinder b -> goBinderFull pos b
    b@(PositionedBinder pos' _ _) -> return (pos', b)
    BinaryNoParensBinder (OpBinder pos' op) lhs rhs ->
      case op `M.lookup` valueAliased of
        Just (Qualified mn' (Left alias)) ->
          throwError . errorMessage' pos' $ InvalidOperatorInBinder op (Qualified mn' alias)
        Just (Qualified mn' (Right alias)) ->
          return (pos', ConstructorBinder pos' (Qualified mn' alias) [lhs, rhs])
        Nothing ->
          throwError . errorMessage' pos' . UnknownName $ fmap ValOpName op
    BinaryNoParensBinder{} ->
      internalError "BinaryNoParensBinder has no OpBinder"
    other -> return (pos, other)

  goType2 :: SourceSpan -> SourceType -> DesugarM SourceType
  goType2 pos = \case
    ParensInType _ t -> goType2 pos t
    TypeOp ann2 op ->
      case op `M.lookup` typeAliased of
        Just alias ->
          return $ TypeConstructor ann2 alias
        Nothing ->
          throwError . errorMessage' pos $ UnknownName $ fmap TyOpName op
    other -> return other

needsReorder, needsFinish :: Int
needsReorder = 1
needsFinish = 2

-- | Which of the rebracketing steps a declaration needs: 'needsReorder' if it
-- has operator chains to reorder, 'needsFinish' if it has anything to do with
-- parentheses, operators or anonymous arguments. Lets us skip the traversals
-- for declarations that have none of them.
needs :: Declaration -> Int
needs = fd
  where
  (fd, _, _, _, _) = everythingOnValues (.|.) goDecl goExpr goBinder (const 0) (const 0)

  goDecl = \case
    DataDeclaration _ _ _ args ctors -> tys (mapMaybe snd args ++ concatMap (map snd . dataCtorFields) ctors)
    ExternDeclaration _ _ ty -> ty1 ty
    TypeClassDeclaration _ _ args implies _ _ -> tys (mapMaybe snd args ++ concatMap constraintArgs implies)
    TypeInstanceDeclaration _ _ _ _ _ cs _ ts _ -> tys (concatMap constraintArgs cs ++ ts)
    TypeSynonymDeclaration _ _ args ty -> tys (mapMaybe snd args ++ [ty])
    TypeDeclaration (TypeDeclarationData _ _ ty) -> ty1 ty
    KindDeclaration _ _ _ ty -> ty1 ty
    ExternDataDeclaration _ _ ty -> ty1 ty
    _ -> 0

  goExpr = \case
    BinaryNoParens{} -> needsReorder .|. needsFinish
    Op{} -> needsFinish
    Parens{} -> needsFinish
    AnonymousArgument -> needsFinish
    TypeClassDictionary c _ _ -> tys (constraintKindArgs c ++ constraintArgs c)
    DeferredDictionary _ ts -> tys ts
    TypedValue _ _ ty -> ty1 ty
    VisibleTypeApp _ ty -> ty1 ty
    _ -> 0

  goBinder = \case
    BinaryNoParensBinder{} -> needsReorder .|. needsFinish
    ParensInBinder{} -> needsFinish
    TypedBinder ty _ -> ty1 ty
    _ -> 0

  tys = foldl' (.|.) 0 . map ty1

  ty1 = everythingOnTypes (.|.) $ \case
    BinaryNoParensType{} -> needsReorder .|. needsFinish
    ParensInType{} -> needsFinish
    TypeOp{} -> needsFinish
    _ -> 0

removeBinaryNoParens :: Expr -> DesugarM Expr
removeBinaryNoParens u
  | isAnonymousArgument u = case u of
                              PositionedValue p _ _ -> rethrowWithPosition p err
                              _ -> err
                            where err = throwError . errorMessage $ IncorrectAnonymousArgument
removeBinaryNoParens (Parens (stripPositionInfo -> BinaryNoParens op l r))
  | isAnonymousArgument r = do arg <- freshIdent'
                               return $ Abs (VarBinder nullSourceSpan arg) $ App (App op l) (Var nullSourceSpan (Qualified ByNullSourcePos arg))
  | isAnonymousArgument l = do arg <- freshIdent'
                               return $ Abs (VarBinder nullSourceSpan arg) $ App (App op (Var nullSourceSpan (Qualified ByNullSourcePos arg))) r
removeBinaryNoParens (BinaryNoParens op l r) = return $ App (App op l) r
removeBinaryNoParens e = return e

stripPositionInfo :: Expr -> Expr
stripPositionInfo (PositionedValue _ _ e) = stripPositionInfo e
stripPositionInfo e = e

externsFixities :: ExternsFile -> [Either ValueFixityRecord TypeFixityRecord]
externsFixities ExternsFile{..} =
  map fromFixity efFixities ++ map fromTypeFixity efTypeFixities
  where

  fromFixity
    :: ExternsFixity
    -> Either ValueFixityRecord TypeFixityRecord
  fromFixity (ExternsFixity assoc prec op name) =
    Left
      ( Qualified (ByModuleName efModuleName) op
      , internalModuleSourceSpan ""
      , Fixity assoc prec
      , name
      )

  fromTypeFixity
    :: ExternsTypeFixity
    -> Either ValueFixityRecord TypeFixityRecord
  fromTypeFixity (ExternsTypeFixity assoc prec op name) =
    Right
      ( Qualified (ByModuleName efModuleName) op
      , internalModuleSourceSpan ""
      , Fixity assoc prec
      , name
      )

collectFixities :: Module -> [Either ValueFixityRecord TypeFixityRecord]
collectFixities (Module _ _ moduleName ds _) = concatMap collect ds
  where
  collect :: Declaration -> [Either ValueFixityRecord TypeFixityRecord]
  collect (ValueFixityDeclaration (SourceAnn ss _) fixity name op) =
    [Left (Qualified (ByModuleName moduleName) op, ss, fixity, name)]
  collect (TypeFixityDeclaration (SourceAnn ss _) fixity name op) =
    [Right (Qualified (ByModuleName moduleName) op, ss, fixity, name)]
  collect _ = []

ensureNoDuplicates
  :: Ord a
  => (a -> SimpleErrorMessage)
  -> [(Qualified a, SourceSpan)]
  -> DesugarM ()
ensureNoDuplicates toError m = go $ sortOn fst m
  where
  go [] = return ()
  go [_] = return ()
  go ((x@(Qualified (ByModuleName mn) op), _) : (y, pos) : _) | x == y =
    rethrow (addHint (ErrorInModule mn)) $
      rethrowWithPosition pos $ throwError . errorMessage $ toError op
  go (_ : rest) = go rest

customOperatorTable
  :: [(Qualified op, Fixity)]
  -> [[(Qualified op, Associativity)]]
customOperatorTable fixities =
  let
    userOps = map (\(name, Fixity a p) -> (name, p, a)) fixities
    sorted = sortOn (Down . (\(_, p, _) -> p)) userOps
    groups = groupBy ((==) `on` (\(_, p, _) -> p)) sorted
  in
    map (map (\(name, _, a) -> (name, a))) groups

updateTypes
  :: forall m
   . Monad m
  => (SourceSpan -> SourceType -> m SourceType)
  -> ( Declaration -> m Declaration
     , SourceSpan -> Expr -> m (SourceSpan, Expr)
     , SourceSpan -> Binder -> m (SourceSpan, Binder)
     )
updateTypes goType = (goDecl, goExpr, goBinder)
  where

  goType' :: SourceSpan -> SourceType -> m SourceType
  goType' = everywhereOnTypesTopDownM . goType

  goDecl :: Declaration -> m Declaration
  goDecl (DataDeclaration sa@(SourceAnn ss _) ddt name args dctors) =
    DataDeclaration sa ddt name
      <$> traverse (traverse (traverse (goType' ss))) args
      <*> traverse (traverseDataCtorFields (traverse (sndM (goType' ss)))) dctors
  goDecl (ExternDeclaration sa@(SourceAnn ss _) name ty) =
    ExternDeclaration sa name <$> goType' ss ty
  goDecl (TypeClassDeclaration sa@(SourceAnn ss _) name args implies deps decls) = do
    implies' <- traverse (overConstraintArgs (traverse (goType' ss))) implies
    args' <- traverse (traverse (traverse (goType' ss))) args
    return $ TypeClassDeclaration sa name args' implies' deps decls
  goDecl (TypeInstanceDeclaration sa@(SourceAnn ss _) na ch idx name cs className tys impls) = do
    cs' <- traverse (overConstraintArgs (traverse (goType' ss))) cs
    tys' <- traverse (goType' ss) tys
    return $ TypeInstanceDeclaration sa na ch idx name cs' className tys' impls
  goDecl (TypeSynonymDeclaration sa@(SourceAnn ss _) name args ty) =
    TypeSynonymDeclaration sa name
      <$> traverse (traverse (traverse (goType' ss))) args
      <*> goType' ss ty
  goDecl (TypeDeclaration (TypeDeclarationData sa@(SourceAnn ss _) expr ty)) =
    TypeDeclaration . TypeDeclarationData sa expr <$> goType' ss ty
  goDecl (KindDeclaration sa@(SourceAnn ss _) sigFor name ty) =
    KindDeclaration sa sigFor name <$> goType' ss ty
  goDecl (ExternDataDeclaration sa@(SourceAnn ss _) name ty) =
    ExternDataDeclaration sa name <$> goType' ss ty
  goDecl other =
    return other

  goExpr :: SourceSpan -> Expr -> m (SourceSpan, Expr)
  goExpr _ e@(PositionedValue pos _ _) = return (pos, e)
  goExpr pos (TypeClassDictionary (Constraint ann name kinds tys info) dicts hints) = do
    kinds' <- traverse (goType' pos) kinds
    tys' <- traverse (goType' pos) tys
    return (pos, TypeClassDictionary (Constraint ann name kinds' tys' info) dicts hints)
  goExpr pos (DeferredDictionary cls tys) = do
    tys' <- traverse (goType' pos) tys
    return (pos, DeferredDictionary cls tys')
  goExpr pos (TypedValue check v ty) = do
    ty' <- goType' pos ty
    return (pos, TypedValue check v ty')
  goExpr pos (VisibleTypeApp v ty) = do
    ty' <- goType' pos ty
    return (pos, VisibleTypeApp v ty')
  goExpr pos other = return (pos, other)

  goBinder :: SourceSpan -> Binder -> m (SourceSpan, Binder)
  goBinder _ e@(PositionedBinder pos _ _) = return (pos, e)
  goBinder pos (TypedBinder ty b) = do
    ty' <- goType' pos ty
    return (pos, TypedBinder ty' b)
  goBinder pos other = return (pos, other)

-- |
-- Checks all the fixity exports within a module to ensure that members aliased
-- by the operators are also exported from the module.
--
-- This pass requires name desugaring and export elaboration to have run first.
--
checkFixityExports
  :: Module
  -> DesugarM Module
checkFixityExports (Module _ _ _ _ Nothing) =
  internalError "exports should have been elaborated before checkFixityExports"
checkFixityExports m@(Module ss _ mn ds (Just exps)) =
  rethrow (addHint (ErrorInModule mn))
    $ rethrowWithPosition ss (traverse_ checkRef exps)
    $> m
  where

  checkRef :: DeclarationRef -> DesugarM ()
  checkRef dr@(ValueOpRef ss' op) =
    for_ (getValueOpAlias op) $ \case
      Left ident ->
        unless (ValueRef ss' ident `elem` exps)
          . throwError . errorMessage' ss'
          $ TransitiveExportError dr [ValueRef ss' ident]
      Right ctor ->
        unless (anyTypeRef (maybe False (elem ctor) . snd))
          . throwError . errorMessage' ss
          $ TransitiveDctorExportError dr [ctor]
  checkRef dr@(TypeOpRef ss' op) =
    for_ (getTypeOpAlias op) $ \ty ->
      unless (anyTypeRef ((== ty) . fst))
        . throwError . errorMessage' ss'
        $ TransitiveExportError dr [TypeRef ss' ty Nothing]
  checkRef _ = return ()

  -- Finds the name associated with a type operator when that type is also
  -- defined in the current module.
  getTypeOpAlias :: OpName 'TypeOpName -> Maybe (ProperName 'TypeName)
  getTypeOpAlias op =
    listToMaybe (mapMaybe (either (const Nothing) go <=< getFixityDecl) ds)
    where
    go (TypeFixity _ (Qualified (ByModuleName mn') ident) op')
      | mn == mn' && op == op' = Just ident
    go _ = Nothing

  -- Finds the value or data constructor associated with an operator when that
  -- declaration is also in the current module.
  getValueOpAlias
    :: OpName 'ValueOpName
    -> Maybe (Either Ident (ProperName 'ConstructorName))
  getValueOpAlias op =
    listToMaybe (mapMaybe (either go (const Nothing) <=< getFixityDecl) ds)
    where
    go (ValueFixity _ (Qualified (ByModuleName mn') ident) op')
      | mn == mn' && op == op' = Just ident
    go _ = Nothing

  -- Tests the exported `TypeRef` entries with a predicate.
  anyTypeRef
    :: ((ProperName 'TypeName, Maybe [ProperName 'ConstructorName]) -> Bool)
    -> Bool
  anyTypeRef f = any (maybe False f . getTypeRef) exps
