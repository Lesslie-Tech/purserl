{-# LANGUAGE GADTs #-}

-- |
-- Monads for type checking and type inference and associated data types
--
module Language.PureScript.TypeChecker.Monad where

import Prelude

import Control.Arrow (second)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Strict (MonadState(..), StateT(..), gets, modify)
import Control.Monad (forM_, guard, join, unless, when, (<=<))
import Control.Exception (Exception, onException, throwIO, try)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (put)
import Control.Monad.Supply (SupplyT(..))
import Control.Monad.Supply.Class (MonadSupply(..))
import Control.Monad.Trans.Class (lift)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Control.Monad.Writer.Class (MonadWriter(..), censor)

import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as M
import Data.IntMap.Strict qualified as IM
import Data.Set qualified as S
import Data.Text (Text, isPrefixOf, unpack)
import Data.List.NonEmpty qualified as NEL

import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (Environment(..), NameKind(..), NameVisibility(..), TypeClassData(..), TypeKind(..))
import Language.PureScript.Errors (Context, ErrorMessageHint, ExportSource, Expr, ImportDeclarationType, MultipleErrors, SimpleErrorMessage(..), SourceAnn, SourceSpan(..), addHint, errorMessage, positionedError, rethrow, warnWithPosition)
import Language.PureScript.Names (Ident(..), ModuleName, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), coerceProperName, disqualify, runIdent, runModuleName, showQualified, toMaybeModuleName)
import Language.PureScript.Pretty.Types (prettyPrintType)
import Language.PureScript.Pretty.Values (prettyPrintValue)
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope(..))
import Language.PureScript.Types (Constraint(..), SourceType, Type(..), srcKindedType, srcTypeVar)
import Language.PureScript.Make.Monad (Make)
import Text.PrettyPrint.Boxes (render)

-- | The concrete monad of the type checker, also used (via 'runCheckPure') by
-- IDE type search.
--
-- It is a single reader-over-IO monad with the checker state, the fresh name
-- supply and the warnings kept in mutable references, because a stack of
-- transformers (StateT / SupplyT / Make) was costing an allocation per layer
-- on every bind. Errors are exceptions; 'catchError' restores the state and
-- supply as they were before the failed action, like the old stack did.
newtype Check a = Check { unCheck :: CheckRefs -> IO a }

data CheckRefs = CheckRefs
  { crState :: !(IORef CheckState)
  , crSupply :: !(IORef Integer)
  , crWarnings :: !(IORef MultipleErrors)
  , crHints :: !(IORef [ErrorMessageHint])
  -- ^ The current error message hint stack. It has its own reference because
  -- every check, inference and unification pushes a hint, which would
  -- otherwise copy the whole 'CheckState' twice.
  }

newtype CheckFailure = CheckFailure MultipleErrors

instance Show CheckFailure where
  show _ = "CheckFailure"

instance Exception CheckFailure

instance Functor Check where
  fmap f (Check m) = Check $ \r -> fmap f (m r)
  {-# INLINE fmap #-}

instance Applicative Check where
  pure a = Check $ \_ -> pure a
  {-# INLINE pure #-}
  Check f <*> Check a = Check $ \r -> f r <*> a r
  {-# INLINE (<*>) #-}
  Check a *> Check b = Check $ \r -> a r *> b r
  {-# INLINE (*>) #-}

instance Monad Check where
  Check m >>= f = Check $ \r -> m r >>= \a -> unCheck (f a) r
  {-# INLINE (>>=) #-}

instance MonadState CheckState Check where
  get = Check $ \r -> readIORef (crState r)
  put s = Check $ \r -> writeIORef (crState r) s
  state f = Check $ \r -> do
    s <- readIORef (crState r)
    case f s of
      (a, s') -> writeIORef (crState r) s' >> pure a
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}

instance MonadError MultipleErrors Check where
  throwError e = Check $ \_ -> throwIO (CheckFailure e)
  catchError (Check m) h = Check $ \r -> do
    s <- readIORef (crState r)
    n <- readIORef (crSupply r)
    hs <- readIORef (crHints r)
    res <- try (m r)
    case res of
      Right a -> pure a
      Left (CheckFailure e) -> do
        writeIORef (crState r) s
        writeIORef (crSupply r) n
        writeIORef (crHints r) hs
        unCheck (h e) r

instance MonadWriter MultipleErrors Check where
  tell w = Check $ \r -> modifyIORef' (crWarnings r) (<> w)
  writer (a, w) = a <$ tell w
  listen (Check m) = Check $ \r -> do
    old <- readIORef (crWarnings r)
    writeIORef (crWarnings r) mempty
    a <- m r `onException` writeIORef (crWarnings r) old
    w <- readIORef (crWarnings r)
    writeIORef (crWarnings r) (old <> w)
    pure (a, w)
  pass (Check m) = Check $ \r -> do
    old <- readIORef (crWarnings r)
    writeIORef (crWarnings r) mempty
    (a, f) <- m r `onException` writeIORef (crWarnings r) old
    w <- readIORef (crWarnings r)
    writeIORef (crWarnings r) (old <> f w)
    pure a

instance MonadSupply Check where
  fresh = Check $ \r -> do
    n <- readIORef (crSupply r)
    writeIORef (crSupply r) (n + 1)
    pure n
  peek = Check $ \r -> readIORef (crSupply r)

-- | Run a 'Check' computation inside the surrounding fresh name supply, handing
-- back the final state. Warnings are reported even if the computation fails.
runCheckInMake :: CheckState -> Check a -> SupplyT Make (a, CheckState)
runCheckInMake st (Check m) = do
  n <- peek
  (res, n', warnings) <- liftIO $ do
    stRef <- newIORef st
    supplyRef <- newIORef n
    warningsRef <- newIORef mempty
    hintsRef <- newIORef []
    res <- try (m (CheckRefs stRef supplyRef warningsRef hintsRef) >>= \a -> (,) a <$> readIORef stRef)
    (,,) res <$> readIORef supplyRef <*> readIORef warningsRef
  lift (tell warnings)
  case res of
    Left (CheckFailure e) -> lift (throwError e)
    Right r -> SupplyT (put n') >> pure r

-- | Run a 'Check' computation purely, for callers outside the build (IDE type
-- search). The IORefs are private to the call, so this is referentially
-- transparent.
runCheckPure :: CheckState -> Check a -> Either MultipleErrors (a, CheckState)
runCheckPure st (Check m) = unsafePerformIO $ do
  stRef <- newIORef st
  supplyRef <- newIORef 0
  warningsRef <- newIORef mempty
  hintsRef <- newIORef []
  try (m (CheckRefs stRef supplyRef warningsRef hintsRef) >>= \a -> (,) a <$> readIORef stRef) >>= \case
    Left (CheckFailure e) -> pure (Left e)
    Right r -> pure (Right r)
{-# NOINLINE runCheckPure #-}

newtype UnkLevel = UnkLevel (NEL.NonEmpty Unknown)
  deriving (Eq, Show)

-- This instance differs from the NEL instance in that longer but otherwise
-- equal paths are LT rather than GT. An extended path puts it *before* its root.
instance Ord UnkLevel where
  compare (UnkLevel a) (UnkLevel b) =
    go (NEL.toList a) (NEL.toList b)
    where
    go [] [] = EQ
    go _  [] = LT
    go [] _  = GT
    go (x:xs) (y:ys) =
      compare x y <> go xs ys

-- | A substitution of unification variables for types.
data Substitution = Substitution
  { substType :: IM.IntMap SourceType
  -- ^ Type substitution
  , substUnsolved :: IM.IntMap (UnkLevel, SourceType)
  -- ^ Unsolved unification variables with their level (scope ordering) and kind
  , substNames :: IM.IntMap Text
  -- ^ The original names of unknowns
  }

insertUnkName :: Unknown -> Text -> Check ()
insertUnkName u t = do
  modify (\s ->
            s { checkSubstitution =
                  (checkSubstitution s) { substNames =
                                            IM.insert u t $ substNames $ checkSubstitution s
                                        }
              }
         )

lookupUnkName :: Unknown -> Check (Maybe Text)
lookupUnkName u = gets $ IM.lookup u . substNames . checkSubstitution

-- | An empty substitution
emptySubstitution :: Substitution
emptySubstitution = Substitution IM.empty IM.empty IM.empty

-- | State required for type checking
data CheckState = CheckState
  { checkEnv :: Environment
  -- ^ The current @Environment@
  , checkNextType :: Int
  -- ^ The next type unification variable
  , checkNextSkolem :: Int
  -- ^ The next skolem variable
  , checkNextSkolemScope :: Int
  -- ^ The next skolem scope constant
  , checkCurrentModule :: Maybe ModuleName
  -- ^ The current module
  , checkCurrentModuleImports ::
      [ ( SourceAnn
        , ModuleName
        , ImportDeclarationType
        , Maybe ModuleName
        , M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ExportSource)
        )
      ]
  -- ^ The current module imports and their exported types.
  -- Newtype constructors have to be in scope for some Coercible constraints to
  -- be solvable, so we need to know which constructors are imported and whether
  -- they are actually defined in or re-exported from the imported modules.
  , checkSubstitution :: Substitution
  -- ^ The current substitution
  , checkConstructorImportsForCoercible :: S.Set (ModuleName, Qualified (ProperName 'ConstructorName))
  -- ^ Newtype constructors imports required to solve Coercible constraints.
  -- We have to keep track of them so that we don't emit unused import warnings.
  , checkUndefinedNames :: S.Set (Qualified Ident)
  -- ^ Superset of the names currently bound with 'Undefined' visibility, so
  -- that making a binding group visible doesn't have to traverse the whole environment.
  }

-- | Create an empty @CheckState@
emptyCheckState :: Environment -> CheckState
emptyCheckState env = CheckState env 0 0 0 Nothing [] emptySubstitution mempty mempty

-- | Unification variables
type Unknown = Int

-- | Temporarily bind a collection of names to values
bindNames
  :: M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -> Check a
  -> Check a
bindNames newNames action = do
  orig <- get
  let undef = S.fromList [ k | (k, (_, _, Undefined)) <- M.toList newNames ]
  modify $ \st -> st { checkEnv = (checkEnv st) { names = newNames `mUnionLeftBiasRightLarger` (names . checkEnv $ st) }
                     , checkUndefinedNames = undef <> checkUndefinedNames st }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { names = names . checkEnv $ orig }
                     , checkUndefinedNames = checkUndefinedNames orig }
  return a

-- | Temporarily bind a collection of names to types
bindTypes
  :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
  -> Check a
  -> Check a
bindTypes newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { types = newNames `mUnionLeftBiasRightLarger` (types . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { types = types . checkEnv $ orig } }
  return a

mUnionLeftBiasRightLarger small large = M.unionWith (\_ s -> s) large small

-- | Temporarily bind a collection of names to types
withScopedTypeVars
  :: ModuleName
  -> [(Text, SourceType)]
  -> Check a
  -> Check a
withScopedTypeVars mn ks ma = do
  orig <- get
  forM_ ks $ \(name, _) ->
    when (Qualified (ByModuleName mn) (ProperName name) `M.member` types (checkEnv orig)) $
      tell . errorMessage $ ShadowedTypeVar name
  bindTypes (M.fromList (map (\(name, k) -> (Qualified (ByModuleName mn) (ProperName name), (k, ScopedTypeVar))) ks)) ma

withErrorMessageHint
  :: ErrorMessageHint
  -> Check a
  -> Check a
withErrorMessageHint hint (Check action) = Check $ \r -> do
  orig <- readIORef (crHints r)
  writeIORef (crHints r) (hint : orig)
  -- The hint stack is kept both for errors, which get the hint added, and for
  -- 'getHints', which gives good error messages during instance resolution. On
  -- failure the stack is restored by the 'catchError' that handles the error.
  res <- try (action r)
  case res of
    Right a -> writeIORef (crHints r) orig >> pure a
    Left (CheckFailure e) -> throwIO (CheckFailure (addHint hint e))

-- | These hints are added at the front, so the most nested hint occurs
-- at the front, but the simplifier assumes the reverse order.
getHints :: Check [ErrorMessageHint]
getHints = Check $ \r -> reverse <$> readIORef (crHints r)

rethrowWithPositionTC
  :: SourceSpan
  -> Check a
  -> Check a
rethrowWithPositionTC pos = withErrorMessageHint (positionedError pos)

warnAndRethrowWithPositionTC
  :: SourceSpan
  -> Check a
  -> Check a
warnAndRethrowWithPositionTC pos = rethrowWithPositionTC pos . warnWithPosition pos

-- | Temporarily make a collection of type class dictionaries available
withTypeClassDictionaries
  :: [NamedDict]
  -> Check a
  -> Check a
withTypeClassDictionaries entries action = do
  orig <- get

  let mentries =
        M.fromListWith (M.unionWith (M.unionWith (<>)))
          [ (qb, M.singleton className (M.singleton tcdValue (pure entry)))
          | entry@TypeClassDictionaryInScope{ tcdValue = tcdValue@(Qualified qb _), tcdClassName = className }
              <- entries
          ]

  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = M.unionWith (M.unionWith (M.unionWith (<>))) (typeClassDictionaries . checkEnv $ st) mentries } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = typeClassDictionaries . checkEnv $ orig } }
  return a

-- | Get the currently available map of type class dictionaries
getTypeClassDictionaries
  :: Check (M.Map QualifiedBy (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict))))
getTypeClassDictionaries = gets $ typeClassDictionaries . checkEnv

-- | Lookup type class dictionaries in a module.
lookupTypeClassDictionaries
  :: QualifiedBy
  -> Check (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict)))
lookupTypeClassDictionaries mn = gets $ fromMaybe M.empty . M.lookup mn . typeClassDictionaries . checkEnv

-- | Lookup type class dictionaries in a module.
lookupTypeClassDictionariesForClass
  :: QualifiedBy
  -> Qualified (ProperName 'ClassName)
  -> Check (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict))
lookupTypeClassDictionariesForClass mn cn = fromMaybe M.empty . M.lookup cn <$> lookupTypeClassDictionaries mn

-- | Temporarily bind a collection of names to local variables
bindLocalVariables
  :: [(SourceSpan, Ident, SourceType, NameVisibility)]
  -> Check a
  -> Check a
bindLocalVariables bindings =
  bindNames (M.fromList $ flip map bindings $ \(ss, name, ty, visibility) -> (Qualified (BySourcePos $ spanStart ss) name, (ty, Private, visibility)))

-- | Temporarily bind a collection of names to local type variables
bindLocalTypeVariables
  :: ModuleName
  -> [(ProperName 'TypeName, SourceType)]
  -> Check a
  -> Check a
bindLocalTypeVariables moduleName bindings =
  bindTypes (M.fromList $ flip map bindings $ \(pn, kind) -> (Qualified (ByModuleName moduleName) pn, (kind, LocalTypeVariable)))

-- | Update the visibility of all names to Defined
makeBindingGroupVisible :: Check ()
makeBindingGroupVisible = do
  undef <- gets checkUndefinedNames
  unless (S.null undef) $ do
    modifyEnv $ \e -> e { names = foldr (M.adjust (\(ty, nk, _) -> (ty, nk, Defined))) (names e) (S.toList undef) }
    modify $ \st -> st { checkUndefinedNames = mempty }

-- | Update the visibility of all names to Defined in the scope of the provided action
withBindingGroupVisible :: Check a -> Check a
withBindingGroupVisible action = preservingNames $ makeBindingGroupVisible >> action

-- | Perform an action while preserving the names from the @Environment@.
preservingNames :: Check a -> Check a
preservingNames action = do
  orig <- gets (names . checkEnv)
  origUndef <- gets checkUndefinedNames
  a <- action
  modifyEnv $ \e -> e { names = orig }
  modify $ \st -> st { checkUndefinedNames = origUndef }
  return a

-- | Lookup the type of a value by name in the @Environment@
lookupVariable :: Qualified Ident -> Check SourceType
lookupVariable qual = do
  env <- getEnv
  case M.lookup qual (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined (disqualify qual)
    Just (ty, _, _) -> return ty

-- | Lookup the visibility of a value by name in the @Environment@
getVisibility :: Qualified Ident -> Check NameVisibility
getVisibility qual = do
  env <- getEnv
  case M.lookup qual (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined (disqualify qual)
    Just (_, _, vis) -> return vis

-- | Assert that a name is visible
checkVisibility :: Qualified Ident -> Check ()
checkVisibility name@(Qualified _ var) = do
  vis <- getVisibility name
  case vis of
    Undefined -> throwError . errorMessage $ CycleInDeclaration var
    _ -> return ()

-- | Lookup the kind of a type by name in the @Environment@
lookupTypeVariable
  :: ModuleName
  -> Qualified (ProperName 'TypeName)
  -> Check SourceType
lookupTypeVariable currentModule (Qualified qb name) = do
  env <- getEnv
  case M.lookup (Qualified qb' name) (types env) of
    Nothing -> throwError . errorMessage $ UndefinedTypeVariable name
    Just (k, _) -> return k
  where
  qb' = ByModuleName $ case qb of
    ByModuleName m -> m
    BySourcePos _ -> currentModule

-- | Get the current @Environment@
getEnv :: Check Environment
getEnv = gets checkEnv

-- | Get locally-bound names in context, to create an error message.
getLocalContext :: Check Context
getLocalContext = do
  env <- getEnv
  return [ (ident, ty') | (Qualified (BySourcePos _) ident@Ident{}, (ty', _, Defined)) <- M.toList (names env) ]

-- | Update the @Environment@
putEnv :: Environment -> Check ()
putEnv env = modify (\s -> s { checkEnv = env })

-- | Modify the @Environment@
modifyEnv :: (Environment -> Environment) -> Check ()
modifyEnv f = modify (\s -> s { checkEnv = f (checkEnv s) })

-- | Make an assertion, failing with an error message
guardWith :: MultipleErrors -> Bool -> Check ()
guardWith _ True = return ()
guardWith e False = throwError e

capturingSubstitution
  :: (a -> Substitution -> b)
  -> Check a
  -> Check b
capturingSubstitution f ma = do
  a <- ma
  subst <- gets checkSubstitution
  return (f a subst)

withFreshSubstitution
  :: Check a
  -> Check a
withFreshSubstitution ma = do
  orig <- get
  modify $ \st -> st { checkSubstitution = emptySubstitution }
  a <- ma
  modify $ \st -> st { checkSubstitution = checkSubstitution orig }
  return a

withoutWarnings
  :: Check a
  -> Check (a, MultipleErrors)
withoutWarnings = censor (const mempty) . listen

unsafeCheckCurrentModule :: Check ModuleName
unsafeCheckCurrentModule = gets checkCurrentModule >>= \case
  Nothing -> internalError "No module name set in scope"
  Just name -> pure name

debugEnv :: Environment -> [String]
debugEnv env = join
  [ debugTypes env
  , debugTypeSynonyms env
  , debugTypeClasses env
  , debugTypeClassDictionaries env
  , debugDataConstructors env
  , debugNames env
  ]

debugType :: Type a -> String
debugType = init . prettyPrintType 100

debugConstraint :: Constraint a -> String
debugConstraint (Constraint ann clsName kinds args _) =
  debugType $ foldl (TypeApp ann) (foldl (KindApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) kinds) args

debugTypes :: Environment -> [String]
debugTypes = go <=< M.toList . types
  where
  go (qual, (srcTy, which)) = do
    let
      ppTy = prettyPrintType 100 srcTy
      name = showQualified runProperName qual
      decl = case which of
        DataType _ _ _    -> "data"
        TypeSynonym       -> "type"
        ExternData _      -> "extern"
        LocalTypeVariable -> "local"
        ScopedTypeVar     -> "scoped"
    guard (not ("Prim" `isPrefixOf` name))
    pure $ decl <> " " <> unpack name <> " :: " <> init ppTy

debugNames :: Environment -> [String]
debugNames = fmap go . M.toList . names
  where
  go (qual, (srcTy, _, _)) = do
    let
      ppTy = prettyPrintType 100 srcTy
      name = showQualified runIdent qual
    unpack name <> " :: " <> init ppTy

debugDataConstructors :: Environment -> [String]
debugDataConstructors = fmap go . M.toList . dataConstructors
  where
  go (qual, (_, _, ty, _)) = do
    let
      ppTy = prettyPrintType 100 ty
      name = showQualified runProperName qual
    unpack name <> " :: " <> init ppTy

debugTypeSynonyms :: Environment -> [String]
debugTypeSynonyms = fmap go . M.toList . typeSynonyms
  where
  go (qual, (binders, subTy)) = do
    let
      vars = unwords $ flip fmap binders $ \case
               (v, Just k) -> "(" <> unpack v <> " :: " <> init (prettyPrintType 100 k) <> ")"
               (v, Nothing) -> unpack v
      ppTy = prettyPrintType 100 subTy
      name = showQualified runProperName qual
    "type " <> unpack name <> " " <> vars <> " = " <> init ppTy

debugTypeClassDictionaries :: Environment -> [String]
debugTypeClassDictionaries = go . typeClassDictionaries
  where
  go tcds = do
    (mbModuleName, classes) <- M.toList tcds
    (className, instances) <- M.toList classes
    (ident, dicts) <- M.toList instances
    let
      moduleName = maybe "" (\m -> "[" <> runModuleName m <> "] ") (toMaybeModuleName mbModuleName)
      className' = showQualified runProperName className
      ident' = showQualified runIdent ident
      kds = unwords $ fmap ((\a -> "@(" <> a <> ")") . debugType) $ tcdInstanceKinds $ NEL.head dicts
      tys = unwords $ fmap ((\a -> "(" <> a <> ")") . debugType) $ tcdInstanceTypes $ NEL.head dicts
    pure $ "dict " <> unpack moduleName <> unpack className' <> " " <> unpack ident' <> " (" <> show (length dicts) <> ")" <> " " <> kds <> " " <> tys

debugTypeClasses :: Environment -> [String]
debugTypeClasses = fmap go . M.toList . typeClasses
  where
  go (className, tc) = do
    let
      className' = showQualified runProperName className
      args = unwords $ (\(a, b) -> "(" <> debugType (maybe (srcTypeVar a) (srcKindedType (srcTypeVar a)) b) <> ")") <$> typeClassArguments tc
    "class " <> unpack className' <> " " <> args

debugValue :: Expr -> String
debugValue = init . render . prettyPrintValue 100

debugSubstitution :: Substitution -> [String]
debugSubstitution (Substitution solved unsolved names) =
  concat
    [ fmap go1 (IM.toList solved)
    , fmap go2 (IM.toList unsolved')
    , fmap go3 (IM.toList names)
    ]
  where
  unsolved' =
    IM.filterWithKey (\k _ -> IM.notMember k solved) unsolved

  go1 (u, ty) =
    "?" <> show u <> " = " <> debugType ty

  go2 (u, (_, k)) =
    "?" <> show u <> " :: " <> debugType k

  go3 (u, t) =
    unpack t <> show u
