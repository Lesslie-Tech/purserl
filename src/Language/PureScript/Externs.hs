{-# Language DeriveAnyClass #-}
{-# Language NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
-- |
-- This module generates code for \"externs\" files, i.e. files containing only
-- foreign import declarations.
--
module Language.PureScript.Externs
  ( ExternsFile(..)
  , ExternsImport(..)
  , ExternsFixity(..)
  , ExternsTypeFixity(..)
  , ExternsDeclaration(..)
  , externsIsCurrentVersion
  , moduleToExternsFile
  , applyExternsFileToEnvironment
  , externsFileName
  , DB(..)
  , DBOpaque(..)
  , dbOpaqueDiffDiffIgnoringExportsListChanges
  ) where

import Prelude

import Data.Binary (Binary(..))
import Data.Binary qualified as Binary
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.List (foldl', find, intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as M
import Data.Map.Merge.Strict qualified as M
import Language.PureScript.Erl qualified as Purserl
import Language.PureScript.Make.Cache qualified as Cache
import GHC.Generics (Generic)

-- import Language.PureScript.AST (Associativity, Declaration(..), DeclarationRef(..), Fixity(..), ImportDeclarationType, Module(..), NameSource(..), Precedence, SourceSpan, pattern TypeFixityDeclaration, pattern ValueFixityDeclaration, getTypeOpRef, getValueOpRef)
import Language.PureScript.AST
import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Crash (internalError)
-- import Language.PureScript.Environment (DataDeclType, Environment(..), FunctionalDependency, NameKind(..), NameVisibility(..), TypeClassData(..), TypeKind(..), dictTypeName, makeTypeClassData)
import Language.PureScript.Environment
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope(..))
-- import Language.PureScript.Types (SourceConstraint, SourceType, srcInstanceType)
import Language.PureScript.Types

import Debug.Trace
import PrettyPrint
import Control.Monad.Trans.State.Strict hiding (get, put, modify)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Class (modify)
import Control.Monad
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Data.Monoid
import Data.Semigroup
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Exts qualified as GHCExts
import Unsafe.Coerce (unsafeCoerce)
import Language.PureScript.Names
import qualified Language.PureScript.Names as N
import Data.Foldable
import Language.PureScript.Roles (Role)
import Language.PureScript.Interning (Intern(..))
import qualified Data.ByteString.UTF8 as BS8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (hashWithSalt)
import qualified Data.ByteArray.Encoding as BAE

import System.IO.Unsafe (unsafePerformIO)
import           System.Environment (lookupEnv)

-- | The data which will be serialized to an externs file
data ExternsFile = ExternsFile
  -- NOTE: Make sure to keep `efVersion` as the first field in this
  -- record, so the hand-written Binary instance's encoding can be checked
  -- for its version independent of the remaining format
  { efVersion :: Text
  -- ^ The externs version
  , efModuleName :: ModuleName
  -- ^ Module name
  , efExports :: [DeclarationRef]
  -- ^ List of module exports
  , efImports :: [ExternsImport]
  -- ^ List of module imports
  , efFixities :: [ExternsFixity]
  -- ^ List of operators and their fixities
  , efTypeFixities :: [ExternsTypeFixity]
  -- ^ List of type operators and their fixities
  , efDeclarations :: [ExternsDeclaration]
  -- ^ List of type and value declaration
  , efSourceSpan :: SourceSpan
  -- ^ Source span for error reporting
  , efUpstreamCacheShapes :: M.Map ModuleName DBOpaque
  -- ^ Shapes of things dependend upon by this module
  , efOurCacheShapes :: DBOpaque
  -- ^ Shapes of things in this module
  } deriving (Show, Generic, NFData)

instance Intern ExternsFile

instance Binary ExternsFile where
  put (ExternsFile a b c d e f g h i j) =
    put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j
  get = ExternsFile <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Eq ExternsFile where
  a == b = Binary.encode a == Binary.encode b

-- | A module import in an externs file
data ExternsImport = ExternsImport
  {
  -- | The imported module
    eiModule :: ModuleName
  -- | The import type: regular, qualified or hiding
  , eiImportType :: ImportDeclarationType
  -- | The imported-as name, for qualified imports
  , eiImportedAs :: Maybe ModuleName
  } deriving (Show, Generic, NFData)

instance Intern ExternsImport

instance Binary ExternsImport where
  put (ExternsImport a b c) = put a >> put b >> put c
  get = ExternsImport <$> get <*> get <*> get

-- | A fixity declaration in an externs file
data ExternsFixity = ExternsFixity
  {
  -- | The associativity of the operator
    efAssociativity :: Associativity
  -- | The precedence level of the operator
  , efPrecedence :: Precedence
  -- | The operator symbol
  , efOperator :: OpName 'ValueOpName
  -- | The value the operator is an alias for
  , efAlias :: Qualified (Either Ident (ProperName 'ConstructorName))
  } deriving (Show, Generic, NFData)

instance Intern ExternsFixity

instance Binary ExternsFixity where
  put (ExternsFixity a b c d) = put a >> put b >> put c >> put d
  get = ExternsFixity <$> get <*> get <*> get <*> get

-- | A type fixity declaration in an externs file
data ExternsTypeFixity = ExternsTypeFixity
  {
  -- | The associativity of the operator
    efTypeAssociativity :: Associativity
  -- | The precedence level of the operator
  , efTypePrecedence :: Precedence
  -- | The operator symbol
  , efTypeOperator :: OpName 'TypeOpName
  -- | The value the operator is an alias for
  , efTypeAlias :: Qualified (ProperName 'TypeName)
  } deriving (Show, Generic, NFData)

instance Intern ExternsTypeFixity

instance Binary ExternsTypeFixity where
  put (ExternsTypeFixity a b c d) = put a >> put b >> put c >> put d
  get = ExternsTypeFixity <$> get <*> get <*> get <*> get

-- | A type or value declaration appearing in an externs file
data ExternsDeclaration =
  -- | A type declaration
    EDType
      { edTypeName                :: ProperName 'TypeName
      , edTypeKind                :: SourceType
      , edTypeDeclarationKind     :: TypeKind
      }
  -- | A type synonym
  | EDTypeSynonym
      { edTypeSynonymName         :: ProperName 'TypeName
      , edTypeSynonymArguments    :: [(Text, Maybe SourceType)]
      , edTypeSynonymType         :: SourceType
      }
  -- | A data constructor
  | EDDataConstructor
      { edDataCtorName            :: ProperName 'ConstructorName
      , edDataCtorOrigin          :: DataDeclType
      , edDataCtorTypeCtor        :: ProperName 'TypeName
      , edDataCtorType            :: SourceType
      , edDataCtorFields          :: [Ident]
      }
  -- | A value declaration
  | EDValue
      { edValueName               :: Ident
      , edValueType               :: SourceType
      }
  -- | A type class declaration
  | EDClass
      { edClassName               :: ProperName 'ClassName
      , edClassTypeArguments      :: [(Text, Maybe SourceType)]
      , edClassMembers            :: [(Ident, SourceType)]
      , edClassConstraints        :: [SourceConstraint]
      , edFunctionalDependencies  :: [FunctionalDependency]
      , edIsEmpty                 :: Bool
      }
  -- | An instance declaration
  | EDInstance
      { edInstanceClassName       :: Qualified (ProperName 'ClassName)
      , edInstanceName            :: Ident
      , edInstanceForAll          :: [(Text, SourceType)]
      , edInstanceKinds           :: [SourceType]
      , edInstanceTypes           :: [SourceType]
      , edInstanceConstraints     :: Maybe [SourceConstraint]
      , edInstanceChain           :: Maybe ChainId
      , edInstanceChainIndex      :: Integer
      , edInstanceNameSource      :: NameSource
      , edInstanceSourceSpan      :: SourceSpan
      }
  deriving (Show, Generic, NFData)

instance Intern ExternsDeclaration

instance Binary ExternsDeclaration where
  put = \case
    EDType a b c -> putWord8 0 >> put a >> put b >> put c
    EDTypeSynonym a b c -> putWord8 1 >> put a >> put b >> put c
    EDDataConstructor a b c d e -> putWord8 2 >> put a >> put b >> put c >> put d >> put e
    EDValue a b -> putWord8 3 >> put a >> put b
    EDClass a b c d e f -> putWord8 4 >> put a >> put b >> put c >> put d >> put e >> put f
    EDInstance a b c d e f g h i j -> putWord8 5 >> put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j
  get = getWord8 >>= \case
    0 -> EDType <$> get <*> get <*> get
    1 -> EDTypeSynonym <$> get <*> get <*> get
    2 -> EDDataConstructor <$> get <*> get <*> get <*> get <*> get
    3 -> EDValue <$> get <*> get
    4 -> EDClass <$> get <*> get <*> get <*> get <*> get <*> get
    5 -> EDInstance <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
    n -> fail ("Binary ExternsDeclaration: invalid tag " <> show n)

-- | Check whether the version in an externs file matches the currently running
-- version.
externsIsCurrentVersion :: ExternsFile -> Bool
externsIsCurrentVersion ef =
  efVersion ef == Purserl.versionString

-- | Convert an externs file back into a module
applyExternsFileToEnvironment :: ExternsFile -> Environment -> Environment
applyExternsFileToEnvironment ExternsFile{..} = flip (foldl' applyDecl) efDeclarations
  where
  applyDecl :: Environment -> ExternsDeclaration -> Environment
  applyDecl env (EDType pn kind tyKind) = env { types = M.insert (qual pn) (kind, tyKind) (types env) }
  applyDecl env (EDTypeSynonym pn args ty) = env { typeSynonyms = M.insert (qual pn) (args, ty) (typeSynonyms env) }
  applyDecl env (EDDataConstructor pn dTy tNm ty nms) = env { dataConstructors = M.insert (qual pn) (dTy, tNm, ty, nms) (dataConstructors env) }
  applyDecl env (EDValue ident ty) = env { names = M.insert (Qualified (ByModuleName efModuleName) ident) (ty, External, Defined) (names env) }
  applyDecl env (EDClass pn args members cs deps tcIsEmpty) = env { typeClasses = M.insert (qual pn) (makeTypeClassData args members cs deps tcIsEmpty) (typeClasses env) }
  applyDecl env (EDInstance className ident vars kinds tys cs ch idx ns ss) =
    env { typeClassDictionaries =
            updateMap
              (updateMap (M.insertWith (<>) (qual ident) (pure dict)) className)
              (ByModuleName efModuleName) (typeClassDictionaries env) }
    where
    dict :: NamedDict
    dict = TypeClassDictionaryInScope ch idx (qual ident) [] className vars kinds tys cs instTy

    updateMap :: (Ord k, Monoid a) => (a -> a) -> k -> M.Map k a -> M.Map k a
    updateMap f = M.alter (Just . f . fold)

    instTy :: Maybe SourceType
    instTy = case ns of
      CompilerNamed -> Just $ srcInstanceType ss vars className tys
      UserNamed -> Nothing

  qual :: a -> Qualified a
  qual = Qualified (ByModuleName efModuleName)


-- Declarations suitable for caching, where things like SourcePos are removed, and each ctor is isolated
-- Only type-level details, for when ctors aren't used.
data CSDataDeclarationTypeOnly =
  CSDataDeclarationTypeOnly
    { csDataDeclDataOrNewtype :: DataDeclType
    , csDataDeclName :: ProperName 'TypeName
    , csDataDeclTargs :: [(Text, Maybe SourceType)]
    , csDataDeclKind :: Maybe CSKindDeclaration
    , csDataDeclRole :: Maybe CSRoleDeclaration
    }
  deriving (Show, Generic, Eq, NFData)

-- Only value-level details. Only needed when ctors are used. Not useful without its corresponding CSDataDeclarationTypeOnly.
data CSDataConstructorDeclaration
  = CSDataConstructorDeclaration
    { csDataCtorName :: !(ProperName 'ConstructorName)
    , csDataCtorFields :: ![(Ident, Type ())]
    }
  deriving (Show, Eq, Generic, NFData)

-- Both type-level and value-level details together, for when ctors are in use.
data CSDataDeclarationWithCtors =
  CSDataDeclarationWithCtors
    { csDataDeclTypeOnly ::  CSDataDeclarationTypeOnly
    , csDataDeclCtorDecls :: [CSDataConstructorDeclaration]
    }
  deriving (Show, Generic, Eq, NFData)


instance Binary CSDataDeclarationTypeOnly where
  put (CSDataDeclarationTypeOnly a b c d e) = put a >> put b >> put c >> put d >> put e
  get = CSDataDeclarationTypeOnly <$> get <*> get <*> get <*> get <*> get

instance Binary CSDataConstructorDeclaration where
  put (CSDataConstructorDeclaration a b) = put a >> put b
  get = CSDataConstructorDeclaration <$> get <*> get

instance Binary CSDataDeclarationWithCtors where
  put (CSDataDeclarationWithCtors a b) = put a >> put b
  get = CSDataDeclarationWithCtors <$> get <*> get

  -- |
  -- A type synonym declaration (name, arguments, type)
  --
data CSTypeSynonymDeclaration = CSTypeSynonymDeclaration (ProperName 'TypeName) [(Text, Maybe (Type ()))] (Type ()) ToCSDB (Maybe CSKindDeclaration)
  deriving (Show, Generic, Eq, NFData)


instance Binary CSTypeSynonymDeclaration where
  put (CSTypeSynonymDeclaration a b c d e) = put a >> put b >> put c >> put d >> put e
  get = CSTypeSynonymDeclaration <$> get <*> get <*> get <*> get <*> get
  -- |
  -- A kind signature declaration
  --
data CSKindDeclaration = CSKindDeclaration (Type ())
  deriving (Show, Generic, Eq, NFData)


instance Binary CSKindDeclaration where
  put (CSKindDeclaration a) = put a
  get = CSKindDeclaration <$> get
  -- |
  -- A role declaration (name, roles)
  --
data CSRoleDeclaration = CSRoleDeclaration [Role]
  deriving (Show, Generic, Eq, NFData)


instance Binary CSRoleDeclaration where
  put (CSRoleDeclaration a) = put a
  get = CSRoleDeclaration <$> get
  -- |
  -- A value declaration (name, top-level binders, optional guard, value)
  --
data CSValueDeclaration = CSValueDeclaration NameKind Int (Type ()) ToCSDB
  deriving (Show, Generic, Eq, NFData)


instance Binary CSValueDeclaration where
  put (CSValueDeclaration a b c d) = put a >> put b >> put c >> put d
  get = CSValueDeclaration <$> get <*> get <*> get <*> get

  -- |
  -- A foreign import declaration (name, type)
  --
data CSExternDeclaration = CSExternDeclaration ToCSDB
  deriving (Show, Generic, Eq, NFData)


instance Binary CSExternDeclaration where
  put (CSExternDeclaration a) = put a
  get = CSExternDeclaration <$> get
  -- |
  -- A data type foreign import (name, kind)
  --
data CSExternDataDeclaration = CSExternDataDeclaration ToCSDB
  deriving (Show, Generic, Eq, NFData)


instance Binary CSExternDataDeclaration where
  put (CSExternDataDeclaration a) = put a
  get = CSExternDataDeclaration <$> get
  -- |
  -- A fixity declaration
  --
data CSOpFixity = CSOpFixity Fixity (Qualified Ident)
  deriving (Show, Generic, Eq, NFData)


instance Binary CSOpFixity where
  put (CSOpFixity a b) = put a >> put b
  get = CSOpFixity <$> get <*> get
data CSCtorFixity = CSCtorFixity Fixity (Qualified (ProperName 'ConstructorName))
  deriving (Show, Generic, Eq, NFData)


instance Binary CSCtorFixity where
  put (CSCtorFixity a b) = put a >> put b
  get = CSCtorFixity <$> get <*> get
data CSTyOpFixity = CSTyOpFixity Fixity (Qualified (ProperName 'TypeName))
  deriving (Show, Generic, Eq, NFData)


instance Binary CSTyOpFixity where
  put (CSTyOpFixity a b) = put a >> put b
  get = CSTyOpFixity <$> get <*> get
  -- |
  -- A type class declaration (name, argument, implies, member declarations)
  --
data CSTypeClassDeclaration = CSTypeClassDeclaration [(Text, Maybe (Type ()))] ([Constraint ()], ToCSDB) [FunctionalDependency] [CSTypeDeclaration]
  deriving (Show, Generic, Eq, NFData)


instance Binary CSTypeClassDeclaration where
  put (CSTypeClassDeclaration a b c d) = put a >> put b >> put c >> put d
  get = CSTypeClassDeclaration <$> get <*> get <*> get <*> get

data CSTypeDeclaration = CSTypeDeclaration Ident (Type ()) ToCSDB
  deriving (Show, Generic, Eq, NFData)


instance Binary CSTypeDeclaration where
  put (CSTypeDeclaration a b c) = put a >> put b >> put c
  get = CSTypeDeclaration <$> get <*> get <*> get
  -- |
  -- A type instance declaration (instance chain, chain index, name,
  -- dependencies, class name, instance types, member declarations)
  --
  -- The first @SourceAnn@ serves as the annotation for the entire
  -- declaration, while the second @SourceAnn@ serves as the
  -- annotation for the type class and its arguments.
data CSTypeInstanceDeclaration = CSTypeInstanceDeclaration (ChainId, Integer) ToCSDB (Qualified (ProperName 'ClassName)) ToCSDB (CSTypeInstanceBody, ToCSDB)
  deriving (Show, Generic, Eq, NFData)


instance Binary CSTypeInstanceDeclaration where
  put (CSTypeInstanceDeclaration a b c d e) = put a >> put b >> put c >> put d >> put e
  get = CSTypeInstanceDeclaration <$> get <*> get <*> get <*> get <*> get

data CSTypeInstanceBody
  = CSDerivedInstance
  | CSNewtypeInstance
  | CSExplicitInstance
  deriving (Show, Eq, Generic, NFData)


instance Binary CSTypeInstanceBody where
  put = \case
    CSDerivedInstance -> putWord8 0
    CSNewtypeInstance -> putWord8 1
    CSExplicitInstance -> putWord8 2
  get = getWord8 >>= \case
    0 -> pure CSDerivedInstance
    1 -> pure CSNewtypeInstance
    2 -> pure CSExplicitInstance
    n -> fail ("Binary CSTypeInstanceBody: invalid tag " <> show n)

data ToCSDB
  = ToCSDB (M.Map ModuleName ToCSDBInner)
  deriving (Show, Eq, Generic, NFData)

runToCSDB :: ToCSDB -> M.Map ModuleName ToCSDBInner
runToCSDB (ToCSDB a) = a


instance Binary ToCSDB where
  put (ToCSDB a) = put a
  get = ToCSDB <$> get

instance Semigroup ToCSDB where
  ToCSDB a <> ToCSDB b = ToCSDB (M.unionWith (<>) a b)

instance Monoid ToCSDB where
  mempty = ToCSDB mempty

data ToCSDBInner
  = ToCSDBInner
    -- TODO[drathier]: all these Qualified contain SourcePos if it's referring to something in the current module. We generally don't want to store SourcePos, but since it's only local, we're already rebuilding the module at that point, so I'll leave it in.
    { _referencedCtors :: M.Map RunIdent ()
    -- NOTE[drathier]: it's likely that we only care about the types of ctors, not types themselves, as using ctors means we care about the type shape changing, but if we just refer to the type we probably don't care what its internal structure is.
    , _referencedTypes :: M.Map (ProperName 'TypeName) ()
    , _referencedTypeOp :: M.Map (OpName 'TypeOpName) ()
    , _referencedTypeClass :: M.Map (ProperName 'ClassName) ()
    , _referencedValues :: M.Map RunIdent ()
    , _referencedValueOp :: M.Map (OpName 'ValueOpName) ()
    }
  deriving (Show, Eq, Generic, NFData)

-- NOTE[drathier]: some idents are run and re-packaged before we get here, so just matching a flat ident won't get you everything you need :( So we run the idents and wrap them up again
newtype RunIdent = RunIdent T.Text
  deriving (Show, Eq, Ord, Generic, NFData)

instance Intern RunIdent

instance Binary RunIdent where
  put (RunIdent a) = put a
  get = RunIdent <$> get

toRunIdent ident =
  -- TODO[drathier]: this makes me sad, what's going on here? Somehow (Ident "a") and (GenIdent (Just "a") 42) result in the same variable name in source. Why isn't the second one "$a42", like when using runIdent?
  RunIdent $
    case ident of
      GenIdent (Just name) _ -> name
      _ -> runIdent ident

-- TODO[drathier]: the type class instance decls have the same name; do they all overwrite each other in the cache? Do I need to qualify them, or skip them?


instance Binary ToCSDBInner where
  put (ToCSDBInner a1 a2 a3 a4 a5 a6) = put a1 >> put a2 >> put a3 >> put a4 >> put a5 >> put a6
  get = ToCSDBInner <$> get <*> get <*> get <*> get <*> get <*> get

instance Semigroup ToCSDBInner where
  ToCSDBInner a1 a2 a3 a4 a5 a6 <> ToCSDBInner b1 b2 b3 b4 b5 b6 = ToCSDBInner (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)

instance Monoid ToCSDBInner where
  mempty = ToCSDBInner mempty mempty mempty mempty mempty mempty

-- | An ephemeral, non-persisted memoization cache, used only to speed up
-- 'storeTypeRefs's traversal of a declaration's (often heavily reused, e.g.
-- a big record row threaded through many binds of a do-block) referenced
-- types. This must never become part of 'ToCSDB' itself: 'ToCSDB' is
-- 'Binary'-serialized straight into the persisted cache-shape hash, and a
-- mutable 'IORef' has no meaningful serialized form there.
--
-- Scoping this correctly matters for two independent reasons:
--
-- 1. purs rebuilds multiple modules concurrently (see the 'C.QSem'-bounded
--    'C.fork' pool in "Language.PureScript.Make"), so a single global cache
--    shared across modules would race.
-- 2. Each top-level declaration in a module gets its own freshly-'mempty'd
--    'DB'/'ToCSDB' accumulator (see 'findDeps'); a cache hit only ever
--    means "skip re-walking this subtree", which is safe *within* the one
--    accumulator that subtree's references need to land in, but would
--    silently drop references from a *later* declaration's own (separate)
--    accumulator if the same physical type node had already been marked
--    "seen" while processing an *earlier* declaration.
--
-- So a fresh 'TypeRefsMemo' is created once per top-level declaration (see
-- its call site in 'findDeps') and threaded through every 'ToCS'
-- computation done on that one declaration's behalf, including into any
-- declarations nested inside it (e.g. via 'DataBindingGroupDeclaration') --
-- those aren't given their own separate accumulator, so sharing the memo
-- with them is exactly as safe as sharing it within any other part of
-- processing that same outer declaration.
-- | A plain list of erased pointers to nodes already seen, compared via
-- 'ptrEq'. Deliberately NOT 'StableName'-based: 'makeStableName' interns
-- into GHC's single, process-wide stable-name table, which turned out to be
-- both much slower per-call and to scale terribly under real concurrency
-- (measured: ~14x slower single-threaded than plain
-- 'reallyUnsafePtrEquality#', and essentially flat past 8 threads instead of
-- scaling with core count -- ~58x slower overall at the concurrency this
-- compiler actually runs at).
--
-- The size is capped at 'typeRefsMemoCapLimit': past that many DISTINCT
-- entries, 'checkAndMarkSeen' always reports "not seen" (a safe miss, see
-- its docs) rather than keep scanning/growing an unbounded list. A plain
-- uncapped list is quadratic in the number of distinct nodes a declaration
-- touches (measured: 4s at 50,000 distinct nodes vs. 0.04s capped/bucketed),
-- and while the intended target (a several-hundred-field record decoder
-- chain) is comfortably under the cap, nothing guarantees every declaration
-- in every module is that small. Capping bounds the worst case to a small
-- fixed constant (roughly @typeRefsMemoCapLimit^2@ comparisons total) no
-- matter how large a declaration gets, at the cost of only memoizing the
-- first 'typeRefsMemoCapLimit' distinct nodes it touches -- for anything
-- past that, we simply fall back to the same behaviour as if this memo
-- didn't exist for the remainder of that one declaration.
typeRefsMemoCapLimit :: Int
typeRefsMemoCapLimit = 1024

data TypeRefsMemoState = TypeRefsMemoState {-# UNPACK #-} !Int [GHCExts.Any]

type TypeRefsMemo = IORef TypeRefsMemoState

-- | Create a fresh, empty memo cache. @NOINLINE@ alone is not enough to stop
-- GHC's full-laziness pass from floating the 'unsafePerformIO' out of the
-- lambda into a single shared top-level CAF -- it only blocks inlining at
-- call sites, not floating within this definition's own body. The dummy
-- argument must actually be forced ('seq') so the body is no longer
-- provably independent of it; only then does every logical call get its own
-- distinct 'IORef', which matters since 'moduleToExternsFile' runs
-- concurrently across modules and a shared cache would both race and leak
-- for the lifetime of the process.
newTypeRefsMemo :: a -> TypeRefsMemo
newTypeRefsMemo x = x `seq` unsafePerformIO (newIORef (TypeRefsMemoState 0 []))
{-# NOINLINE newTypeRefsMemo #-}

-- | Has this exact (by pointer identity, not structural equality) node
-- already been fully processed under this memo? If so, mark nothing
-- further to do; if not, record it as seen from now on (unless the cap has
-- been reached, in which case this always reports "not seen" without
-- touching the list further -- see 'typeRefsMemoCapLimit').
--
-- This is purely a performance heuristic riding on 'storeTypeRefs' being
-- idempotent: a "miss" always just means we (re)do the work, and a "hit"
-- is always safe to skip, since inserting the same references into the
-- ambient 'ToCSDB' a second time would be a no-op anyway (they land in
-- @Map k ()@-shaped fields). The argument is forced before comparing --
-- 'ptrEq' does not evaluate its arguments, so comparing against an unforced
-- thunk would compare the thunk's address, not the value it reduces to,
-- silently defeating the memo.
checkAndMarkSeen :: TypeRefsMemo -> Type a -> Bool
checkAndMarkSeen memoRef !t = unsafePerformIO $ do
  let t' = unsafeCoerce t :: GHCExts.Any
  TypeRefsMemoState sz seen <- readIORef memoRef
  if sz >= typeRefsMemoCapLimit
    then pure False
    else if any (ptrEq t') seen
      then pure True
      else do
        writeIORef memoRef (TypeRefsMemoState (sz + 1) (t' : seen))
        pure False
{-# NOINLINE checkAndMarkSeen #-}

-- | The monad 'ToCS' runs in: an ambient, per-declaration 'TypeRefsMemo'
-- (see above) on top of the actual accumulated 'ToCSDB' state.
type CS = ReaderT TypeRefsMemo (State ToCSDB)

-- NOTE[drathier]: yes, I know Language.PureScript.AST.Traversals exists, but since not all things are newtype wrapped, I would've missed some cases using that, e.g. kind signatures
class ToCS a b | a -> b where
  toCS :: a -> CS b

instance ToCS DataConstructorDeclaration CSDataConstructorDeclaration where
  toCS (DataConstructorDeclaration _ ctorName ctorFields) =
    do
      ctorFields2 <-
        ctorFields
          & traverse
            (\(ident, typeWithSrcAnn) ->
              do
                let t2 = void typeWithSrcAnn
                toCS typeWithSrcAnn
                pure (ident, t2)
            )
      pure $
        CSDataConstructorDeclaration
          ctorName
          ctorFields2

instance ToCS GuardedExpr () where
  toCS (GuardedExpr guards expr) =
    do
      nguards <- traverse toCS guards
      nexpr <- toCS expr
      pure ()

instance ToCS Guard () where
  toCS guard =
    case guard of
        ConditionGuard expr -> toCS expr
        PatternGuard binder expr -> do
          toCS binder
          toCS expr

instance ToCS Expr () where
  toCS expr =
    case expr of
      Literal _ lit -> toCS lit
      UnaryMinus _ e -> toCS e
      BinaryNoParens e1 e2 e3 -> do
        toCS e1
        toCS e2
        toCS e3
      Parens e -> toCS e
      Accessor _ e -> toCS e
      ObjectUpdate e obj -> do
        toCS e
        traverse_ (traverse_ toCS) obj
      ObjectUpdateNested e tree -> do
         toCS e
         traverse_ toCS tree
      Abs binder e -> do
        toCS binder
        toCS e
      App e1 e2 -> do
        toCS e1
        toCS e2
      VisibleTypeApp e sourceType -> do
        toCS e
        toCS sourceType
      Unused e -> toCS e
      Var _ qIdent -> csdbPutIdent qIdent
      Op _ qValueOpName -> csdbPutValueOp qValueOpName
      IfThenElse e1 e2 e3 -> do
        toCS e1
        toCS e2
        toCS e3
      Constructor _ qCtorName -> csdbPutCtor qCtorName
      Case exprs cases -> do
        traverse_ toCS exprs
        traverse_ toCS cases
      TypedValue _ e sourceType -> do
        toCS e
        toCS sourceType

      Let whereOrLet decls inExpr -> do
        toCS decls
        toCS inExpr
      Do _ doNotationElems -> do
        traverse_ toCS doNotationElems

      Ado _ doNotationElems inExpr -> do
        traverse_ toCS doNotationElems
        toCS inExpr

      TypeClassDictionary _ _ _ -> internalError "[drathier]: should be unreachable, all TypeClassDictionary ctors should have been expanded already"
      DeferredDictionary _ _ -> internalError "[drathier]: should be unreachable, all DeferredDictionary ctors should have been expanded already"
      DerivedInstancePlaceholder _ _ -> internalError "[drathier]: should be unreachable, all DerivedInstancePlaceholder ctors should have been expanded already"
      AnonymousArgument -> internalError "[drathier]: should be unreachable, all AnonymousArgument ctors should have been expanded already"
      Hole _ -> internalError "[drathier]: should be unreachable, all Hole ctors should have been expanded already"
      PositionedValue _ _ e -> toCS e

instance ToCS a () => ToCS (Literal a) () where
  toCS expr =
    case expr of
      NumericLiteral _ -> pure ()
      StringLiteral _ -> pure ()
      CharLiteral _ -> pure ()
      BooleanLiteral _ -> pure ()
      ArrayLiteral arr -> traverse_ toCS arr
      ObjectLiteral obj -> traverse_ (traverse_ toCS) obj


instance ToCS DoNotationElement () where
  toCS doNotationElement =
    case doNotationElement of
      DoNotationValue e -> toCS e
      DoNotationBind binder e -> do
        toCS binder
        toCS e
      DoNotationLet decls -> do
        toCS decls
      PositionedDoNotationElement _ _ elem -> toCS elem

instance ToCS Binder () where
  toCS binder =
    case binder of
      NullBinder -> pure ()
      LiteralBinder _ literal -> toCS literal
      VarBinder _ _ -> pure ()
      ConstructorBinder _ ctorName binders -> do
        csdbPutCtor ctorName
        traverse_ toCS binders
      OpBinder _ _ -> internalError "[drathier]: should be unreachable, all OpBinder ctors should have been desugared already"
      BinaryNoParensBinder _ _ _ -> internalError "[drathier]: should be unreachable, all BinaryNoParensBinder ctors should have been desugared already"
      ParensInBinder _ -> internalError "[drathier]: should be unreachable, all ParensInBinder ctors should have been desugared already"
      NamedBinder _ _ innerBinder -> toCS innerBinder
      PositionedBinder _ _ innerBinder -> toCS innerBinder
      TypedBinder sourceType innerBinder -> do
        toCS sourceType
        toCS innerBinder

instance ToCS CaseAlternative () where
  toCS (CaseAlternative binders guardedExprs) = do
    traverse_ toCS binders
    traverse_ toCS guardedExprs

instance ToCS [Declaration] () where
  toCS ds = traverse_ (toCS . snd) (do
    let env = internalError "TODO[drathier]: missing env in ToCS"
    let mn = ModuleName "TODO[drathier]: missing mn in ToCS"
    -- TODO[drathier]: lifting CSDB values out of DB like this feels weird. Should CSDB and DB be the same type? Should we use ToCS for e.g. findDeps too?
    findDeps mn env ds)

instance ToCS DB () where
  toCS (DB dataOrNewtypeDeclsTypeOnly dataOrNewtypeDeclsWithCtors ctorTypes typeSynonymDecls valueDecls externDecls externDataDecls opFixity ctorFixity tyOpFixity tyClassDecls tyClassInstanceDecls _exports) = do
    dataOrNewtypeDeclsTypeOnly & traverse_ (traverse_ (\(csdb, _) -> modify (<> csdb)))
    dataOrNewtypeDeclsWithCtors & traverse_ (traverse_ (\(csdb, _) -> modify (<> csdb)))
    typeSynonymDecls & traverse_ (traverse_ (\(CSTypeSynonymDeclaration _ _ _ csdb _) -> modify (<> csdb)))
    valueDecls & traverse_ (traverse_ (\(CSValueDeclaration _ _ _ csdb) -> modify (<> csdb)))
    externDecls & traverse_ (traverse_ (\(CSExternDeclaration csdb) -> modify (<> csdb)))
    externDataDecls & traverse_ (traverse_ (\(CSExternDataDeclaration csdb) -> modify (<> csdb)))
    tyClassDecls & traverse_ (traverse_ (\(CSTypeClassDeclaration _ (_, csdb) _ tyDeps) ->
      do
        modify (<> csdb)
        tyDeps & traverse_ (\(CSTypeDeclaration _ _ csdb) -> modify (<> csdb))
      ))
    tyClassInstanceDecls & traverse_ (traverse_ (\(CSTypeInstanceDeclaration _ csdb1 _ csdb2 (_, csdb3)) ->
        modify (<> csdb1 <> csdb2 <> csdb3)
      ))

instance ToCS (Type SourceAnn) () where
  toCS t = storeTypeRefs t

instance ToCS (Type ()) () where
  toCS = storeTypeRefs

instance ToCS (Constraint SourceAnn) () where
  toCS = storeConstraintTypes

storeConstraintTypes :: Constraint a -> CS ()
storeConstraintTypes (Constraint _ refTypeClass kindArgs targs mdata) = do
  csdbPutTypeClass refTypeClass
  traverse_ storeTypeRefs kindArgs
  traverse_ storeTypeRefs targs

-- CSDB put helpers

csdbPutCtor :: Qualified (ProperName 'ConstructorName) -> CS ()
csdbPutCtor =
  csdbPutHelper
   (\v refCtor ->
      v {
        _referencedCtors =
         M.insert
          (RunIdent $ runProperName refCtor)
          ()
          (_referencedCtors v)
       }
   )

csdbPutType :: Qualified (ProperName 'TypeName) -> CS ()
csdbPutType =
  csdbPutHelper
   (\v refType ->
      v {
        _referencedTypes =
         M.insert
          refType
          ()
          (_referencedTypes v)
       }
   )

csdbPutTypeOp :: Qualified (OpName 'TypeOpName) -> CS ()
csdbPutTypeOp =
  csdbPutHelper
   (\v refTypeOp ->
      v {
        _referencedTypeOp =
         M.insert
          refTypeOp
          ()
          (_referencedTypeOp v)
       }
   )

csdbPutTypeClass :: Qualified (ProperName 'ClassName) -> CS ()
csdbPutTypeClass =
  csdbPutHelper
   (\v refTypeClass ->
      v {
        _referencedTypeClass =
         M.insert
          refTypeClass
          ()
          (_referencedTypeClass v)
       }
   )

csdbPutIdent :: Qualified Ident -> CS ()
csdbPutIdent =
  csdbPutHelper
   (\v ident ->
      v {
        _referencedValues =
         M.insert
          (toRunIdent ident)
          ()
          (_referencedValues v)
       }
   )

csdbPutValueOp :: Qualified (OpName 'ValueOpName) -> CS ()
csdbPutValueOp =
  csdbPutHelper
   (\v refOp ->
      v {
        _referencedValueOp =
         M.insert
          refOp
          ()
          (_referencedValueOp v)
       }
   )


csdbPutHelper :: (ToCSDBInner -> t -> ToCSDBInner) -> Qualified t -> CS ()
csdbPutHelper f (Qualified qBy ref) =
  modify
    (\(ToCSDB outer) ->
      ToCSDB $
      case qBy of
        BySourcePos _ -> outer
        ByModuleName mn ->
          M.alter
            (\maybeInner ->
              Just (f (maybeInner & fromMaybe mempty) ref)
            )
            mn
            outer
    )


-- DB put helpers

dbPutDataDeclaration :: ProperName 'TypeName -> ToCSDB -> CSDataDeclarationWithCtors -> State DB ()
dbPutDataDeclaration tname nctorsDB csDataDeclaration@(CSDataDeclarationWithCtors typelevel ctors) =
  modify
   (\db ->
    db
      {
        _dataOrNewtypeDeclsTypeOnly =
           M.insertWith (<>)
            tname
            [(nctorsDB, typelevel)]
            (_dataOrNewtypeDeclsTypeOnly db)
      , _dataOrNewtypeDeclsFull =
           M.insertWith (<>)
            tname
            [(nctorsDB, csDataDeclaration)]
            (_dataOrNewtypeDeclsFull db)
      , _ctorTypes =
         foldr
           (\(CSDataConstructorDeclaration ctorName _) m ->
             M.insert
              (RunIdent $ runProperName ctorName)
              tname
              m
            )
            (_ctorTypes db)
            ctors
      }
   )

dbPutTypeSynonymDeclaration :: ProperName 'TypeName -> CSTypeSynonymDeclaration -> State DB ()
dbPutTypeSynonymDeclaration tname csDataDeclaration =
  modify
   (\db ->
    db
      {
        _typeSynonymDecls =
           M.insertWith (<>)
            tname
            [csDataDeclaration]
            (_typeSynonymDecls db)
      }
   )


dbPutValueDeclaration :: Ident -> CSValueDeclaration -> State DB ()
dbPutValueDeclaration ident csValueDeclaration =
  modify
   (\db ->
    db
      {
        _valueDecls =
           M.insertWith (<>)
            (toRunIdent ident)
            [csValueDeclaration]
            (_valueDecls db)
      }
   )


dbPutExternDeclaration :: Ident -> CSExternDeclaration -> State DB ()
dbPutExternDeclaration ident csExternDeclaration =
  modify
   (\db ->
    db
      {
        _externDecls =
           M.insertWith (<>)
            (toRunIdent ident)
            [csExternDeclaration]
            (_externDecls db)
      }
   )


dbPutExternDataDeclaration :: ProperName 'TypeName -> CSExternDataDeclaration -> State DB ()
dbPutExternDataDeclaration tname csExternDataDeclaration =
  modify
   (\db ->
    db
      {
        _externDataDecls =
           M.insertWith (<>)
            tname
            [csExternDataDeclaration]
            (_externDataDecls db)
      }
   )


dbPutOpFixity :: OpName 'ValueOpName -> CSOpFixity -> State DB ()
dbPutOpFixity tname csOpFixity =
  modify
   (\db ->
    db
      {
        _opFixity =
           M.insertWith (<>)
            tname
            [csOpFixity]
            (_opFixity db)
      }
   )


dbPutCtorFixity :: OpName 'ValueOpName -> CSCtorFixity -> State DB ()
dbPutCtorFixity opName csCtorFixity =
  modify
   (\db ->
    db
      {
        _ctorFixity =
           M.insertWith (<>)
            opName
            [csCtorFixity]
            (_ctorFixity db)
      }
   )


dbPutTyOpFixity :: OpName 'TypeOpName -> CSTyOpFixity -> State DB ()
dbPutTyOpFixity tyOpName csTyOpFixity =
  modify
   (\db ->
    db
      {
        _tyOpFixity =
           M.insertWith (<>)
            tyOpName
            [csTyOpFixity]
            (_tyOpFixity db)
      }
   )

dbPutTypeClassDeclaration :: ProperName 'ClassName -> CSTypeClassDeclaration -> State DB ()
dbPutTypeClassDeclaration className csTypeClassDeclaration =
  modify
   (\db ->
    db
      {
        _tyClassDecls =
           M.insertWith (<>)
            className
            [csTypeClassDeclaration]
            (_tyClassDecls db)
      }
   )

dbPutTypeInstanceDeclaration :: Ident -> CSTypeInstanceDeclaration -> State DB ()
dbPutTypeInstanceDeclaration ident csTypeInstanceDeclaration =
  modify
   (\db ->
    db
      {
        _tyClassInstanceDecls =
           M.insertWith (<>)
            (toRunIdent ident)
            [csTypeInstanceDeclaration]
            (_tyClassInstanceDecls db)
      }
   )

storeTypeRefs :: Type a -> CS ()
storeTypeRefs t = do
  memo <- ask
  let alreadySeen = checkAndMarkSeen memo t
  unless alreadySeen $ storeTypeRefsGo t

storeTypeRefsGo :: Type a -> CS ()
storeTypeRefsGo t =
  case t of
    TUnknown _ _ -> pure ()
    TypeVar _ _ -> pure ()
    TypeLevelString _ _ -> pure ()
    TypeLevelInt _ _ -> pure ()
    TypeWildcard _ _ -> pure ()
    TypeConstructor _ refType -> do
      -- [drathier]: this seems to be the type, not the constructor, which makes much more sense at the type level
      csdbPutType refType

    TypeOp _ refTypeOp -> do
      -- TODO[drathier]: is this ctor unused here? docs for it say it's desugared to a TypeConstructor
      csdbPutTypeOp refTypeOp

    TypeApp _ t1 t2 -> do
      storeTypeRefs t1
      storeTypeRefs t2

    KindApp _ t1 t2 -> do
      storeTypeRefs t1
      storeTypeRefs t2

    ForAll _ _ _ mt1 t2 _ -> do
      traverse_ storeTypeRefs mt1
      storeTypeRefs t2

    ConstrainedType _ constraint t1 -> do
      storeConstraintTypes constraint
      storeTypeRefs t1

    Skolem _ _ mt1 _ _ -> do
      traverse_ storeTypeRefs mt1

    REmpty a -> pure ()

    RCons _ _ t1 t2 -> do
      storeTypeRefs t1
      storeTypeRefs t2

    KindedType _ t1 t2 -> do
      storeTypeRefs t1
      storeTypeRefs t2

    BinaryNoParensType _ t1 t2 t3 -> do
      storeTypeRefs t1
      storeTypeRefs t2
      storeTypeRefs t3

    ParensInType _ t1 -> do
      storeTypeRefs t1


data DB
  = DB
    -- what things did we find?
    -- NOTE[drathier]: this gathers a list of direct dependencies, not transitive dependencies, and it doesn't fetch the shape of the dependencies. It's just the set of things we depend on, for us to fetch later.
    -- TODO[drathier]: make sure all these lists are singletons
    -- TODO[drathier]: all that have a ToCSDB should have it separately like in this first row below, and the e.g. CSDataDeclaration should only contain the things that, if they change, should cause a recompile of things depending on this thing
    { _dataOrNewtypeDeclsTypeOnly :: M.Map (ProperName 'TypeName) [(ToCSDB, CSDataDeclarationTypeOnly)]
    , _dataOrNewtypeDeclsFull :: M.Map (ProperName 'TypeName) [(ToCSDB, CSDataDeclarationWithCtors)]
    , _ctorTypes :: M.Map RunIdent (ProperName 'TypeName)
    , _typeSynonymDecls :: M.Map (ProperName 'TypeName) [CSTypeSynonymDeclaration]
    , _valueDecls :: M.Map RunIdent [CSValueDeclaration] -- TODO[drathier]: ToCSDB here too?
    , _externDecls :: M.Map RunIdent [CSExternDeclaration]
    , _externDataDecls :: M.Map (ProperName 'TypeName) [CSExternDataDeclaration]
    , _opFixity :: M.Map (OpName 'ValueOpName) [CSOpFixity]
    , _ctorFixity :: M.Map (OpName 'ValueOpName) [CSCtorFixity]
    , _tyOpFixity :: M.Map (OpName 'TypeOpName) [CSTyOpFixity]
    , _tyClassDecls :: M.Map (ProperName 'ClassName) [CSTypeClassDeclaration]
    , _tyClassInstanceDecls :: M.Map RunIdent [CSTypeInstanceDeclaration]
    , _exports :: ExportSummary
    }
  deriving (Show, Eq, Generic, NFData)

instance Semigroup DB where
  DB a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 <> DB b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 = DB (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12) (a13 <> b13)

instance Monoid DB where
  mempty = DB mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

dbToOpaque :: DB -> DBOpaque
dbToOpaque (DB a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13) =
  DBOpaque
  (a1 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  (a2 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  a3
  (a4 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  (a5 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  (a6 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  (a7 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  (a8 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  (a9 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  (a10 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  (a11 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  (a12 & M.map (\v -> v & Binary.encode & cacheShapeHashFromByteString))
  a13 -- & Binary.encode & cacheShapeHashFromByteString)

data DBOpaque
  = DBOpaque
    -- TODO[drathier]: ToCSDB here is only needed to figure out what we depend on. It shouldn't be needed to be stored anywhere.
    -- what things did we find?
    -- NOTE[drathier]: this gathers a list of direct dependencies, not transitive dependencies, and it doesn't fetch the shape of the dependencies. It's just the set of things we depend on, for us to fetch later.
    -- TODO[drathier]: make sure all these lists are singletons
    -- TODO[drathier]: all that have a ToCSDB should have it separately like in this first row below, and the e.g. CSDataDeclaration should only contain the things that, if they change, should cause a recompile of things depending on this thing
    { _dataOrNewtypeDeclsTypeOnly_opaque :: M.Map (ProperName 'TypeName) CacheShapeHash
    , _dataOrNewtypeDeclsFull_opaque :: M.Map (ProperName 'TypeName) CacheShapeHash
    , _ctorTypes_opaque :: M.Map RunIdent (ProperName 'TypeName)
    , _typeSynonymDecls_opaque :: M.Map (ProperName 'TypeName) CacheShapeHash
    , _valueDecls_opaque :: M.Map RunIdent CacheShapeHash
    , _externDecls_opaque :: M.Map RunIdent CacheShapeHash
    , _externDataDecls_opaque :: M.Map (ProperName 'TypeName) CacheShapeHash
    , _opFixity_opaque :: M.Map (OpName 'ValueOpName) CacheShapeHash
    , _ctorFixity_opaque :: M.Map (OpName 'ValueOpName) CacheShapeHash
    , _tyOpFixity_opaque :: M.Map (OpName 'TypeOpName) CacheShapeHash
    , _tyClassDecls_opaque :: M.Map (ProperName 'ClassName) CacheShapeHash
    , _tyClassInstanceDecls_opaque :: M.Map RunIdent CacheShapeHash
    , _exports_opaque :: ExportSummary -- CacheShapeHash
    }
  deriving (Eq, Generic, NFData)

instance Intern DBOpaque

instance Show DBOpaque where
  show db =
    "DBOpaque{" <>
    intercalate ", " (filter ((/=) "")
      [ if _dataOrNewtypeDeclsTypeOnly_opaque db == mempty then "" else "_dataOrNewtypeDeclsTypeOnly_opaque=" <> show (_dataOrNewtypeDeclsTypeOnly_opaque db)
      , if _dataOrNewtypeDeclsFull_opaque db == mempty then "" else "_dataOrNewtypeDeclsFull_opaque=" <> show (_dataOrNewtypeDeclsFull_opaque db)
      , if _ctorTypes_opaque db == mempty then "" else "_ctorTypes_opaque=" <> show (_ctorTypes_opaque db)
      , if _typeSynonymDecls_opaque db == mempty then "" else "_typeSynonymDecls_opaque=" <> show (_typeSynonymDecls_opaque db)
      , if _valueDecls_opaque db == mempty then "" else "_valueDecls_opaque=" <> show (_valueDecls_opaque db)
      , if _externDecls_opaque db == mempty then "" else "_externDecls_opaque=" <> show (_externDecls_opaque db)
      , if _externDataDecls_opaque db == mempty then "" else "_externDataDecls_opaque=" <> show (_externDataDecls_opaque db)
      , if _opFixity_opaque db == mempty then "" else "_opFixity_opaque=" <> show (_opFixity_opaque db)
      , if _ctorFixity_opaque db == mempty then "" else "_ctorFixity_opaque=" <> show (_ctorFixity_opaque db)
      , if _tyOpFixity_opaque db == mempty then "" else "_tyOpFixity_opaque=" <> show (_tyOpFixity_opaque db)
      , if _tyClassDecls_opaque db == mempty then "" else "_tyClassDecls_opaque=" <> show (_tyClassDecls_opaque db)
      , if _tyClassInstanceDecls_opaque db == mempty then "" else "_tyClassInstanceDecls_opaque=" <> show (_tyClassInstanceDecls_opaque db)
      , if _exports_opaque db == mempty then "" else "_exports_opaque=" <> show (_exports_opaque db)
      ])
    <> "}"


instance Binary DBOpaque where
  put (DBOpaque a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13) =
    put a1 >> put a2 >> put a3 >> put a4 >> put a5 >> put a6 >> put a7 >> put a8 >> put a9 >> put a10 >> put a11 >> put a12 >> put a13
  get = DBOpaque <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Semigroup DBOpaque where
  DBOpaque a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 <> DBOpaque b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 = DBOpaque (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12) (a13 <> b13)

instance Monoid DBOpaque where
  mempty = DBOpaque mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- Fast, non-cryptographic hash: this is a change-detection fingerprint, not
-- a security boundary, so there's no need to pay for a cryptographic hash
-- like the SHA256 this used to be.
cacheShapeHashFromByteString :: ByteString -> CacheShapeHash
cacheShapeHashFromByteString b =
  let
      digest :: (Int, Int)
      digest = (hashWithSalt 0 b, hashWithSalt 1 b)
  in
    digest & show & BS8.fromString & CacheShapeHash

newtype CacheShapeHash = CacheShapeHash BS8.ByteString
  deriving (Show, Eq, Generic, NFData)
  deriving (Semigroup, Monoid) via BS8.ByteString


instance Intern CacheShapeHash where intern = id

instance Binary CacheShapeHash where
  put (CacheShapeHash a) = put a
  get = CacheShapeHash <$> get

dbOpaqueIsctExports :: Show meta => meta -> M.Map ModuleName DBOpaque -> ExportSummary -> DBOpaque -> DBOpaque
dbOpaqueIsctExports meta upstreamDBs (ExportSummary valueName typeName typeOpName typeClass typeClassInstance valueOpName reExportedRefs) (DBOpaque a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13) =
  let
    upstreamReExports =
      M.intersectionWith
        (\innerExportSummary innerDB -> dbOpaqueIsctExports "inner" upstreamDBs innerExportSummary innerDB)
        reExportedRefs
        upstreamDBs
    ourDB =
      DBOpaque
        { _dataOrNewtypeDeclsTypeOnly_opaque = M.intersectionWith (\_ b -> b) typeName a1
        , _dataOrNewtypeDeclsFull_opaque = a2
        , _ctorTypes_opaque = a3
        , _typeSynonymDecls_opaque = M.intersectionWith (\_ b -> b) typeName a4
        , _valueDecls_opaque = M.intersectionWith (\_ b -> b) valueName a5
        , _externDecls_opaque = a6
        , _externDataDecls_opaque = M.intersectionWith (\_ b -> b) typeName a7
        , _opFixity_opaque = M.intersectionWith (\_ b -> b) valueOpName a8
        , _ctorFixity_opaque = M.intersectionWith (\_ b -> b) valueOpName a9
        , _tyOpFixity_opaque = M.intersectionWith (\_ b -> b) typeOpName a10
        , _tyClassDecls_opaque = M.intersectionWith (\_ b -> b) typeClass a11
        , _tyClassInstanceDecls_opaque = M.intersectionWith (\_ b -> b) typeClassInstance a12
        , _exports_opaque = a13
        }
  in
  foldl'
    (\dbSoFar upstreamDB ->
      -- [drathier]: <> is Map union; it keeps left arg on conflict
      dbSoFar <> upstreamDB
    )
    ourDB
    upstreamReExports

dbOpaqueDiffDiffIgnoringExportsListChanges (DBOpaque a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13) (DBOpaque b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13) =
    DBOpaque
      { _dataOrNewtypeDeclsTypeOnly_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a1 b1
      , _dataOrNewtypeDeclsFull_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a2 b2
      , _ctorTypes_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a3 b3
      , _typeSynonymDecls_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a4 b4
      , _valueDecls_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a5 b5
      , _externDecls_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a6 b6
      , _externDataDecls_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a7 b7
      , _opFixity_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a8 b8
      , _ctorFixity_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a9 b9
      , _tyOpFixity_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a10 b10
      , _tyClassDecls_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a11 b11
      , _tyClassInstanceDecls_opaque = M.differenceWith (\x y -> if x == y then Nothing else Just x) a12 b12
      -- NOTE: this field is unfiltered (see `_exports_opaque = a13` in both
      -- `dbOpaqueIsctExports` and `efUpstreamCacheShapes`'s construction), so
      -- comparing it for full equality made *any* addition/removal anywhere in
      -- an upstream module's export list (even of declarations a downstream
      -- module never references) look like a change, forcing needless
      -- downstream rebuilds. Actual removals/renames of declarations a
      -- downstream module does use are already caught by the per-field diffs
      -- above (a used key goes missing via `M.differenceWith`), so this field
      -- can be ignored here, matching this function's name.
      , _exports_opaque = mempty
      }


findDeps :: ModuleName -> Environment -> [Declaration] -> [(Declaration, DB)]
findDeps mn env decls =
  let
    (kindsMap, rolesMap, otherDs) =
      decls
      & flattenDecls
      & foldl (\(akind, arole, aother) d ->
        let
          addKind key value = (M.insert key value akind, arole, aother)
          addRole key value = (akind, M.insert key value arole, aother)
          addOther other = (akind, arole, other : aother)
        in
          case d of
            KindDeclaration _ kindSignatureFor referencedName stype ->
              addKind (kindSignatureFor, referencedName) (CSKindDeclaration (void stype))
            RoleDeclaration (RoleDeclarationData _ tname roles) ->
              addRole tname (CSRoleDeclaration roles)
            d -> addOther d
        )
      (mempty, mempty, mempty)

    getKind :: KindSignatureFor -> ProperName 'TypeName -> Maybe CSKindDeclaration
    getKind kindSignatureFor tname =
      M.lookup (kindSignatureFor, tname) kindsMap

    getRole :: ProperName 'TypeName -> Maybe CSRoleDeclaration
    getRole tname =
      M.lookup tname rolesMap
  in
  otherDs
    <&> (\a -> do
      let memo = newTypeRefsMemo a
      (a, mempty & execState (findDepsImpl getKind getRole mn env memo a))
     )
    & filter (\(_, db) -> db /= mempty)

findDepsImpl
  :: (KindSignatureFor -> ProperName 'TypeName -> Maybe CSKindDeclaration)
  -> (ProperName 'TypeName -> Maybe CSRoleDeclaration)
  -> ModuleName
  -> Environment
  -> TypeRefsMemo
  -> Declaration
  -> State DB ()
findDepsImpl getKind getRole mn env memo d =
  -- data Declaration
  case d of
    -- DataDeclaration SourceAnn DataDeclType (ProperName 'TypeName) [(Text, Maybe SourceType)] [DataConstructorDeclaration]
    DataDeclaration _ dataOrNewtype tname targs ctors -> do
      let (nctorsValue, nctorsDB) = mempty & runState (runReaderT (traverse toCS ctors) memo)

      let mkind =
            case dataOrNewtype of
              Data -> getKind DataSig tname
              Newtype -> getKind NewtypeSig tname

      dbPutDataDeclaration tname nctorsDB
        (CSDataDeclarationWithCtors
          (CSDataDeclarationTypeOnly dataOrNewtype tname targs mkind (getRole tname))
          nctorsValue
        )
      -- pure $ f (show ("DataDeclaration", tname)) $
      --   show ("DataDeclaration", dataOrNewtype, tname, targs, ctors)
    -- DataBindingGroupDeclaration (NEL.NonEmpty Declaration)
    DataBindingGroupDeclaration decls ->
      -- rarely used here, but used by e.g. Data.Void
      traverse_ (findDepsImpl getKind getRole mn env memo) decls

    -- TypeSynonymDeclaration SourceAnn (ProperName 'TypeName) [(Text, Maybe SourceType)] SourceType
    TypeSynonymDeclaration _ tname targs stype -> do
      -- TODO[drathier]: KindedType.purs has a "Just SourceType" targ. I don't know how to handle it here. Right now I'm just storing it as-is.
      let nstype = stype $> ()
      let ntargs = targs <&> fmap (fmap void)
      let nstypeDB = stype & replaceTypeSynonyms (types env) (typeSynonyms env <&> snd) & (`runReaderT` memo) & flip execState mempty
      dbPutTypeSynonymDeclaration tname (CSTypeSynonymDeclaration tname ntargs nstype nstypeDB (getKind TypeSynonymSig tname))

    -- KindDeclaration SourceAnn KindSignatureFor (ProperName 'TypeName) SourceType
    KindDeclaration _ _ _ _ ->
      internalError "[drathier]: should be unreachable, all KindDeclaration ctors should have been filtered out earlier"

    -- RoleDeclaration {-# UNPACK #-} !RoleDeclarationData
    RoleDeclaration _ ->
      -- ASSUMPTION[drathier]: got this compiler error when testing, assuming it to be true forever "Role declarations are only supported for data types, not for type synonyms nor type classes." We'll likely incorrectly  cache things wrt this if this changes in the future. Testing also shows that it works fine for newtypes, so I'm supporting that.
      internalError "[drathier]: should be unreachable, all RoleDeclaration ctors should have been filtered out earlier"
    -- TypeDeclaration {-# UNPACK #-} !TypeDeclarationData
    TypeDeclaration _ ->
      internalError "ASSUMPTION[drathier]: should be unreachable, all TypeDeclaration ctors should have been extracted earlier"
    -- ValueDeclaration {-# UNPACK #-} !(ValueDeclarationData [GuardedExpr])
    ValueDeclaration (ValueDeclarationData _ ident namekind binders exprs) -> do
      -- TODO[drathier]: do we really need expr in here too? Yes, we need to know what modules its value and type refers to at least.
      let !(_, nexprDB) = mempty & runState (runReaderT (traverse_ toCS exprs) memo)
      let tipe = case M.lookup (Qualified (ByModuleName mn) ident) (names env) of
                    Nothing -> internalError "drathier1"
                    Just (ty, _, _) -> void ty
      dbPutValueDeclaration ident (CSValueDeclaration namekind (length binders) tipe nexprDB)

    -- BoundValueDeclaration SourceAnn Binder Expr
    BoundValueDeclaration _ _ _ ->
      internalError "ASSUMPTION[drathier]: should be unreachable, all BoundValueDeclaration ctors should have been desugared earlier"
    -- BindingGroupDeclaration (NEL.NonEmpty ((SourceAnn, Ident), NameKind, Expr))
    BindingGroupDeclaration decls ->
      -- rarely used here, but used by e.g. instance HeytingAlgebra Boolean, since its type class function implementations call eachother (implies calls not)
      traverse_ (findDepsImpl getKind getRole mn env memo . (\((sourceAnn, ident), nameKind, expr) ->
          ValueDeclaration (ValueDeclarationData sourceAnn ident nameKind [] [GuardedExpr [] expr])
        )) decls

    -- ExternDeclaration SourceAnn Ident SourceType
    ExternDeclaration _ ident sourceType -> do
      let !(_, ntypeDB) = mempty & runState (runReaderT (toCS sourceType) memo)
      dbPutExternDeclaration ident (CSExternDeclaration ntypeDB)

    -- ExternDataDeclaration SourceAnn (ProperName 'TypeName) SourceType
    ExternDataDeclaration _ tname sourceType -> do
      let !(_, ntypeDB) = mempty & runState (runReaderT (toCS sourceType) memo)
      dbPutExternDataDeclaration tname (CSExternDataDeclaration ntypeDB)

    -- FixityDeclaration SourceAnn (Either ValueFixity TypeFixity)
    FixityDeclaration _ eitherValueType ->
      case eitherValueType of
        -- ValueFixityDeclaration :: SourceAnn -> Fixity -> Qualified (Either Ident (ProperName 'ConstructorName)) -> OpName 'ValueOpName -> Declaration
        Left (ValueFixity fixity (N.Qualified qBy (Left ident)) localOpName) ->
          dbPutOpFixity localOpName (CSOpFixity fixity (N.Qualified qBy ident))
        Left (ValueFixity fixity (N.Qualified qBy (Right ctor)) localCtorName) ->
          dbPutCtorFixity localCtorName (CSCtorFixity fixity (N.Qualified qBy ctor))
        -- TypeFixityDeclaration :: SourceAnn -> Fixity -> Qualified (ProperName 'TypeName) -> OpName 'TypeOpName -> Declaration
        Right (TypeFixity fixity tname tyOpName) ->
          dbPutTyOpFixity tyOpName (CSTyOpFixity fixity tname)

    -- ImportDeclaration SourceAnn ModuleName ImportDeclarationType (Maybe ModuleName)
    ImportDeclaration _ modu importDeclType mAlias ->
      -- imports are handled before this function runs, so ignored here
      -- TODO[drathier]: we can track the import decl types and malias to see if a module is only ever (including transitive re-exports) imported qualified. Then we'll know the exact subset of values which are in scope. Well, values, not necessarily types.
      pure ()
    -- TypeClassDeclaration SourceAnn (ProperName 'ClassName) [(Text, Maybe SourceType)] [SourceConstraint] [FunctionalDependency] [Declaration]
    TypeClassDeclaration _ className targs constraints fnDeps decls -> do
      ndecls <- decls & traverse (\case
          TypeDeclaration (TypeDeclarationData _ ident tipe) -> do
            let (ntipe, ntipeDB) = mempty & runState (runReaderT (toCS tipe) memo)
            pure $ CSTypeDeclaration ident (void tipe) ntipeDB
          v -> error ("ASSUMPTION[drathier]: The inner declarations in the type class declaration are just TypeDeclarations." ++ show v)
        )

      -- TODO[drathier]: test the constraintKindArgs and constraintData fields of Constraint. I couldn't figure out a source input that would put anything in those fields.

      let (_, nconstraintsdb) = mempty & runState (runReaderT (traverse toCS constraints) memo)
      let nconstraints = void <$> constraints
      let ntargs = targs <&> second ((<$>) void)
      let !_ = nconstraints <&>
            (\case
              Constraint _ _ [] _ _ -> ()
              v -> error ("ASSUMPTION[drathier]: the constraintKindArgs field of constraints for type classes is always empty." ++ show v)
            )
      dbPutTypeClassDeclaration className (CSTypeClassDeclaration ntargs (nconstraints, nconstraintsdb) fnDeps ndecls)

    -- TypeInstanceDeclaration SourceAnn SourceAnn ChainId Integer (Either Text Ident) [SourceConstraint] (Qualified (ProperName 'ClassName)) [SourceType] TypeInstanceBody
    TypeInstanceDeclaration _ _ chainId chainIdIndex eitherTextIdentInstanceName dependencySourceConstraints className instanceSourceTypes derivedNewtypeExplicit -> do
      let !(_, ndependencySourceConstraintsDB) = mempty & runState (runReaderT (traverse toCS dependencySourceConstraints) memo)
      let !(_, ninstanceSourceTypesDB) = mempty & runState (runReaderT (traverse toCS instanceSourceTypes) memo)
      let !(nderivedNewtypeExplicitNoDecls, nderivedNewtypeExplicit) = mempty & runState (runReaderT (
                case derivedNewtypeExplicit of
                  DerivedInstance -> pure CSDerivedInstance
                  NewtypeInstance -> pure CSNewtypeInstance
                  ExplicitInstance decls ->
                    do
                      toCS decls
                      pure CSExplicitInstance
                ) memo)
      let !_ = dependencySourceConstraints <&>
            (\case
              Constraint _ _ [] _ _ -> ()
              v -> error ("ASSUMPTION[drathier]: the constraintKindArgs field of constraints for type class instances is always empty." ++ show v)
            )
      let instanceName =
            case eitherTextIdentInstanceName of
              Left _ -> internalError "ASSUMPTION[drathier]: we'll never get a Text value here; even generated instances have Idents"
              Right v -> v
      dbPutTypeInstanceDeclaration instanceName (CSTypeInstanceDeclaration (chainId, chainIdIndex) ndependencySourceConstraintsDB className ninstanceSourceTypesDB (nderivedNewtypeExplicitNoDecls, nderivedNewtypeExplicit))


replaceTypeSynonyms
  :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
  -> M.Map (Qualified (ProperName 'TypeName)) (Type a) -> Type a -> CS (Type a)
replaceTypeSynonyms typesMap typeSynonymsMap =
  -- NOTE[drathier]: replaceAllTypeSynonyms exists, but I couldn't get it to work in this context.
  -- TODO[drathier]: this shouldn't have to look further than the module we imported the type alias from. Currently it fetches all the way down, because it looks at the Environment, rather than the Externs cache shape. On the other hand, it's unlikely to matter much in practice.
  let f t =
        case t of
          TypeConstructor _ qt@(Qualified (ByModuleName modu) tipe) ->
            -- type aliases (type synonyms)
            case M.lookup qt typeSynonymsMap of
              Just v -> do
                csdbPutType qt
                replaceTypeSynonyms typesMap typeSynonymsMap v
              Nothing | moduIsPrim modu -> csdbPutType qt >> pure t
              Nothing | M.member qt typesMap -> csdbPutType qt >> pure t
              Nothing -> internalError (sShow ("[drathier]: couldn't find upstream type constructor in env", qt, modu, tipe))
          _ -> pure t
  in everywhereOnTypesM f

-- TODO[drathier]: this is very similar to ToCSDBInner, but not quite. This tracks exported values, ToCSDBInner tracks used types.
data ExportSummary =
  ExportSummary
    { _refValue :: M.Map RunIdent ()
    , _refTypeName :: M.Map (ProperName 'TypeName) ()
    , _refTypeOpName :: M.Map (OpName 'TypeOpName) ()
    , _refTypeClass :: M.Map (ProperName 'ClassName) ()
    , _refTypeClassInstance :: M.Map RunIdent ()
    , _refOpName :: M.Map (OpName 'ValueOpName) ()
    -- [drathier]: re-exports of whole modules are desugared to one-by-one export refs, so we don't have to handle them here
    , _reExportRef :: M.Map ModuleName ExportSummary
    } deriving (Show, Eq, Ord, Generic, NFData)

instance Intern ExportSummary

instance Binary ExportSummary where
  put (ExportSummary a1 a2 a3 a4 a5 a6 a7) =
    put a1 >> put a2 >> put a3 >> put a4 >> put a5 >> put a6 >> put a7
  get = ExportSummary <$> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Monoid ExportSummary where
  mempty = ExportSummary mempty mempty mempty mempty mempty mempty mempty

instance Semigroup ExportSummary where
  ExportSummary a1 a2 a3 a4 a5 a6 a7 <> ExportSummary b1 b2 b3 b4 b5 b6 b7 = ExportSummary (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7)

exsumPutTypeOpName ref v =
  v {
    _refTypeOpName =
     M.insert
      ref
      ()
      (_refTypeOpName v)
   }

exsumPutTypeName ref v =
  v {
    _refTypeName =
     M.insert
      ref
      ()
      (_refTypeName v)
   }

exsumPutValue ref v =
  v {
    _refValue =
     M.insert
      ref
      ()
      (_refValue v)
   }

exsumPutOpName ref v =
  v {
    _refOpName =
     M.insert
      ref
      ()
      (_refOpName v)
   }

exsumPutTypeClassRef ref v =
  v {
    _refTypeClass =
     M.insert
      ref
      ()
      (_refTypeClass v)
   }

exsumPutTypeClassInstance ref v =
  v {
    _refTypeClassInstance =
     M.insert
      ref
      ()
      (_refTypeClassInstance v)
   }


findExportedThings :: [DeclarationRef] -> ExportSummary
findExportedThings declRefs =
  foldl findExportedThingsImpl mempty declRefs

findExportedThingsImpl :: ExportSummary -> DeclarationRef -> ExportSummary
findExportedThingsImpl exsum declRef =
  case declRef of
    TypeClassRef _ className -> exsumPutTypeClassRef className exsum
    TypeOpRef _ tyOpName -> exsumPutTypeOpName tyOpName exsum
    TypeRef _ tyName mCtorNames -> exsumPutTypeName tyName exsum
    ValueRef _ ident -> exsumPutValue (toRunIdent ident) exsum
    ValueOpRef _ opName -> exsumPutOpName opName exsum
    TypeInstanceRef _ ident _ -> exsumPutTypeClassInstance (toRunIdent ident) exsum
    ReExportRef _ src ref -> findExportedThingsImpl exsum ref
    ModuleRef _ modu ->
      -- [drathier]: re-exports of whole modules are desugared to one-by-one export refs, so we don't have to handle them here. However, they're still left in, so we have to ignore them here, rather than assert that we never see any value like this here
      exsum

-- | Generate an externs file for all declarations in a module.
--
-- The `Map Ident Ident` argument should contain any top-level `GenIdent`s that
-- were rewritten to `Ident`s when the module was compiled; this rewrite only
-- happens in the CoreFn, not the original module AST, so it needs to be
-- applied to the exported names here also. (The appropriate map is returned by
-- `L.P.Renamer.renameInModule`.)
moduleToExternsFile :: M.Map ModuleName DBOpaque -> Module -> Environment -> M.Map Ident Ident -> ExternsFile
moduleToExternsFile _upstreamDbs (Module _ _ _ _ Nothing) _ _ = internalError "moduleToExternsFile: module exports were not elaborated"
-- data Module = Module SourceSpan [Comment] ModuleName [Declaration] (Maybe [DeclarationRef])
moduleToExternsFile upstreamDBs (Module ss _comments mn decls (Just exports)) env renamedIdents =
  let
    sortDsByCtor :: Foldable f => f Declaration -> M.Map T.Text [Declaration]
    sortDsByCtor dsx =
      foldr sortDsByCtorImpl mempty dsx
    sortDsByCtorImpl :: Declaration -> M.Map T.Text [Declaration] -> M.Map T.Text [Declaration]
    sortDsByCtorImpl d res =
      case d of
        DataDeclaration _ _ _ _ _ -> M.insertWith (<>) "DataDeclaration" [d] res
        DataBindingGroupDeclaration _ -> M.insertWith (<>) "DataBindingGroupDeclaration" [d] res
        TypeSynonymDeclaration _ _ _ _ -> M.insertWith (<>) "TypeSynonymDeclaration" [d] res
        KindDeclaration _ _ _ _ -> M.insertWith (<>) "KindDeclaration" [d] res
        RoleDeclaration _ -> M.insertWith (<>) "RoleDeclaration" [d] res
        TypeDeclaration _ -> M.insertWith (<>) "TypeDeclaration" [d] res
        ValueDeclaration _ -> M.insertWith (<>) "ValueDeclaration" [d] res
        BoundValueDeclaration _ _ _ -> M.insertWith (<>) "BoundValueDeclaration" [d] res
        BindingGroupDeclaration _ -> M.insertWith (<>) "BindingGroupDeclaration" [d] res
        ExternDeclaration _ _ _ -> M.insertWith (<>) "ExternDeclaration" [d] res
        ExternDataDeclaration _ _ _ -> M.insertWith (<>) "ExternDataDeclaration" [d] res
        FixityDeclaration _ _ -> M.insertWith (<>) "FixityDeclaration" [d] res
        ImportDeclaration _ _ _ _ -> M.insertWith (<>) "ImportDeclaration" [d] res
        TypeClassDeclaration _ _ _ _ _ _ -> M.insertWith (<>) "TypeClassDeclaration" [d] res
        TypeInstanceDeclaration _ _ _ _ _ _ _ _ _ -> M.insertWith (<>) "TypeInstanceDeclaration" [d] res

    sds = sortDsByCtor decls


  in


  let shouldCache = not $
        case unsafePerformIO (lookupEnv "PURS_DISABLE_DISK_CACHE") of
          Just "0" -> False
          Just "no" -> False
          Just "false" -> False
          Just "False" -> False
          Just "FALSE" -> False
          Just "" -> False
          Nothing -> False
          _ -> True
  in

  let possiblyImportedTypeAliasesFrom :: M.Map ModuleName () -- [(ProperName 'TypeName)]
      possiblyImportedTypeAliasesFrom =
        decls
        & concatMap (\case
          -- TODO[drathier]: look at importDeclType
          ImportDeclaration _ modu _importDeclType _mAlias ->
            [modu]
          _ -> []
        )
        <&> (,())
        & M.fromList
  in
  let exportedThings = findExportedThings exports in
  -- let safeImports = findQualifiedImportedModules mn efImports upstreamDBs in
  let findDepsRes = if not shouldCache then [] else findDeps mn env decls in
  let dbDeps = foldl (<>) (mempty { _exports = exportedThings }) (snd <$> findDepsRes) in
  let csdbDeps = flip execState mempty $ runReaderT (toCS dbDeps) (newTypeRefsMemo dbDeps) in
  let efOurCacheShapes = dbDeps & dbToOpaque & dbOpaqueIsctExports ("self", mn) upstreamDBs exportedThings in
  -- let efUpstreamReExports = buildEfUpstreamReExports upstream exports mempty mempty in
  -- let !_ = trace (sShow ("###moduleToExternsFile findExportedThings", mn, exportedThings)) () in
{-
  let !_ = trace (show ("###moduleToExternsFile mn", mn)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.BoundValueDeclaration", M.lookup "BoundValueDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.BindingGroupDeclaration", M.lookup "BindingGroupDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.ExternDeclaration", M.lookup "ExternDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.ExternDataDeclaration", M.lookup "ExternDataDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.FixityDeclaration", M.lookup "FixityDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.ImportDeclaration", M.lookup "ImportDeclaration" sds)) () in
-}
  -- let !_ = trace (sShow ("###moduleToExternsFile decls.TypeClassDeclaration", M.lookup "TypeClassDeclaration" sds)) () in
  -- let !_ = trace (sShow ("###moduleToExternsFile decls.TypeInstanceDeclaration", M.lookup "TypeInstanceDeclaration" sds)) () in
{-
  let !_ = trace (show ("###moduleToExternsFile decls.DataDeclaration", M.lookup "DataDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.DataBindingGroupDeclaration", M.lookup "DataBindingGroupDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.TypeSynonymDeclaration", M.lookup "TypeSynonymDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.KindDeclaration", M.lookup "KindDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.RoleDeclaration", M.lookup "RoleDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile decls.TypeDeclaration", M.lookup "TypeDeclaration" sds)) () in
-}
  -- let !_ = trace (sShow ("###moduleToExternsFile decls.ValueDeclaration", M.lookup "ValueDeclaration" sds)) () in
{-
  let !_ = trace (show ("###moduleToExternsFile exports", exports)) () in
  let !_ = trace (show ("###moduleToExternsFile renamedIdents", renamedIdents)) () in
  let !_ = trace (show ("-------")) () in
  let !_ = trace (show ("###moduleToExternsFile findDeps", findDepsRes)) () in
  let !_ = trace (show ("###moduleToExternsFile findDepsDB", dbDeps)) () in -- NOTE[drathier]: this is the beginning of the CacheShape data; i.e. what's the shape of this data, according to anyone who wants to use it
  let !_ = trace (show ("###moduleToExternsFile findDepsCSDB", csdbDeps)) () in -- NOTE[drathier]: this will become the list of things we depend on from other modules, i.e. the things we need to look up, copy in, and diff against the old values to figure out if we should recompile or not
-}

  -- ("WARNING: old is empty",ModuleName "B")
  -- TODO[drathier]: type aliases aren't tracked across deps yet; same bug as with the old attempt. Solved by re-exporting the relevant info. It seems like no expr has the type alias type anywhere; maybe type aliases are always lost? They're available in the ast at least, so we can get at them, even if we have to assume everyone depends on all type aliases always, and possibly same for type classes
  --
  -- Confirmed: this also affects type class superclasses, two or more hops
  -- downstream. `efUpstreamCacheShapes` below only records shapes for modules
  -- directly referenced by this module's own declarations, one hop at a time;
  -- it isn't propagated transitively. So if module A's class TC gains/loses a
  -- superclass, a module C that calls a TC member directly (`thingy = tc`)
  -- correctly rebuilds (C imports A directly), but C's own resulting shape
  -- for `thingy` doesn't change (the type `TC a => a` only embeds TC's name,
  -- not its current superclass set) -- so a module D that only imports C (not
  -- A) never finds out and incorrectly skips its rebuild. See the disabled
  -- second half of "asdf tracks type class super classes across module
  -- boundaries" in tests/TestMake.hs for a reproduction. Fixing this needs
  -- cache-shape propagation to be transitive across the whole dependency
  -- graph, not a local patch here.


  -- TODO[drathier]: handle imports? we only look at what's actually used, so what's the point?
  -- TODO[drathier]: handle exports?
  -- TODO[drathier]: handle re-exports?

  let efUpstreamCacheShapes :: M.Map ModuleName DBOpaque
      efUpstreamCacheShapes =
        if not shouldCache then M.empty else
          let currentDeps :: M.Map ModuleName ToCSDBInner
              currentDeps =
                runToCSDB csdbDeps
                -- TODO[drathier]: we always depend on all imported type aliases. We shouldn't have to do that.
                & M.merge
                  (M.mapMissing (\_ () -> mempty))
                  (M.mapMissing (\_ b -> b))
                  (M.zipWithMatched (\_ () b -> b))
                  possiblyImportedTypeAliasesFrom

                -- don't look for ourselves or built-in modules in the upstream cache
                & M.delete mn
                & M.filterWithKey (\m _ -> not (moduIsPrim m))
          in
          M.merge
            M.dropMissing
            (M.mapMissing (\k a -> error ("[drathier]: cache key only in currentDeps2, missing in build history: " <> show
              ( "key", k
              , "modu", mn
              , "commonKeys", M.keys (M.intersection (void currentDeps) (void upstreamDBs))
              , "currentDepsKeys", M.keys currentDeps
              , "v", a
              ))))
            (M.zipWithMatched
              (\mnDep
                up
                (ToCSDBInner
                  ctors
                  types
                  typeOp
                  typeClasses
                  values
                  valueOp) ->
                let
                    dbCtorToType :: M.Map (ProperName 'TypeName) RunIdent
                    dbCtorToType =
                      _ctorTypes_opaque up
                      & M.toList
                      <&> (\(a,b) -> (b,a))
                      & M.fromList

                    typesRefByCtors :: M.Map (ProperName 'TypeName) ()
                    typesRefByCtors =
                      ctors
                        & M.intersectionWith const (_ctorTypes_opaque up)
                        & M.elems
                        <&> (,())
                        & M.fromList
                in
                -- TODO[drathier]: it would be nice to check that each of the names we tried to look up matched exactly one thing when building this DB

                -- TODO[drathier]: the dry run envvar should still run the caching logic, to see if it would've skipped the recompile, and also compare the rebuilt exts to the cached ones, to see if the rebuild was needed. If there's a mismatch in either direction, print it to stdout so I can debug it later.
                -- case fromMaybe (internalError $ show ("Externs: couldn't find safeImport", mn, mnDep)) $ M.lookup mnDep safeImports of
                   DBOpaque
                      (M.intersectionWith (\_ b -> b) types (_dataOrNewtypeDeclsTypeOnly_opaque up))
                      (M.intersectionWith (\_ b -> b) typesRefByCtors (_dataOrNewtypeDeclsFull_opaque up))
                      (M.intersectionWith (\_ b -> b) ctors (_ctorTypes_opaque up))
                      (_typeSynonymDecls_opaque up) -- (M.intersectionWith (\_ b -> b) types (_typeSynonymDecls up)) -- TODO[drathier]: We don't really know if a type alias was used or not, because type aliases get replaced with the thing they're aliasing before we get here. However, we could look at explicit exports and explicit imports to filter this a bit.
                      (M.intersectionWith (\_ b -> b) values (_valueDecls_opaque up))
                      (M.intersectionWith (\_ b -> b) values (_externDecls_opaque up))
                      (M.intersectionWith (\_ b -> b) types (_externDataDecls_opaque up))
                      (M.intersectionWith (\_ b -> b) valueOp (_opFixity_opaque up))
                      (M.intersectionWith (\_ b -> b) valueOp (_ctorFixity_opaque up))
                      (M.intersectionWith (\_ b -> b) typeOp (_tyOpFixity_opaque up))
                      (M.intersectionWith (\_ b -> b) typeClasses (_tyClassDecls_opaque up))
                      (M.intersectionWith (\_ b -> b) values (_tyClassInstanceDecls_opaque up))
                      (_exports_opaque up)
              )
            )
            upstreamDBs
            currentDeps
          & M.filter (\dbo -> dbo /= (mempty :: DBOpaque))

  in
  -- let !_ = trace (sShow ("###moduleToExternsFile efUpstreamCacheShapes", mn, efUpstreamCacheShapes)) () in
  -- let !_ = trace (sShow ("###moduleToExternsFile efOurCacheShapes", mn, efOurCacheShapes)) () in
  ExternsFile{..}
  where
  efVersion       = Purserl.versionString
  efModuleName    = mn
  efExports       = map renameRef exports
  efImports       = mapMaybe importDecl decls
  efFixities      = mapMaybe fixityDecl decls
  efTypeFixities  = mapMaybe typeFixityDecl decls
  efDeclarations  = concatMap toExternsDeclaration exports
  efSourceSpan    = ss

  fixityDecl :: Declaration -> Maybe ExternsFixity
  fixityDecl (ValueFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsFixity assoc prec op name)) (find ((== Just op) . getValueOpRef) exports)
  fixityDecl _ = Nothing

  typeFixityDecl :: Declaration -> Maybe ExternsTypeFixity
  typeFixityDecl (TypeFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsTypeFixity assoc prec op name)) (find ((== Just op) . getTypeOpRef) exports)
  typeFixityDecl _ = Nothing

  importDecl :: Declaration -> Maybe ExternsImport
  importDecl (ImportDeclaration _ m mt qmn) = Just (ExternsImport m mt qmn)
  importDecl _ = Nothing

  toExternsDeclaration :: DeclarationRef -> [ExternsDeclaration]
  toExternsDeclaration (TypeRef _ pn dctors) =
    case Qualified (ByModuleName mn) pn `M.lookup` types env of
      Nothing -> internalError "toExternsDeclaration: no kind in toExternsDeclaration"
      Just (kind, TypeSynonym)
        | Just (args, synTy) <- Qualified (ByModuleName mn) pn `M.lookup` typeSynonyms env -> [ EDType pn kind TypeSynonym, EDTypeSynonym pn args synTy ]
      Just (kind, ExternData rs) -> [ EDType pn kind (ExternData rs) ]
      Just (kind, tk@(DataType _ _ tys)) ->
        EDType pn kind tk : [ EDDataConstructor dctor dty pn ty args
                            | dctor <- fromMaybe (map fst tys) dctors
                            , (dty, _, ty, args) <- maybeToList (Qualified (ByModuleName mn) dctor `M.lookup` dataConstructors env)
                            ]
      _ -> internalError "toExternsDeclaration: Invalid input"
  toExternsDeclaration (ValueRef _ ident)
    | Just (ty, _, _) <- Qualified (ByModuleName mn) ident `M.lookup` names env
    = [ EDValue (lookupRenamedIdent ident) ty ]
  toExternsDeclaration (TypeClassRef _ className)
    | let dictName = dictTypeName . coerceProperName $ className
    , Just TypeClassData{..} <- Qualified (ByModuleName mn) className `M.lookup` typeClasses env
    , Just (kind, tk) <- Qualified (ByModuleName mn) (coerceProperName className) `M.lookup` types env
    , Just (dictKind, dictData@(DataType _ _ [(dctor, _)])) <- Qualified (ByModuleName mn) dictName `M.lookup` types env
    , Just (dty, _, ty, args) <- Qualified (ByModuleName mn) dctor `M.lookup` dataConstructors env
    = [ EDType (coerceProperName className) kind tk
      , EDType dictName dictKind dictData
      , EDDataConstructor dctor dty dictName ty args
      , EDClass className typeClassArguments ((\(a, b, _) -> (a, b)) <$> typeClassMembers) typeClassSuperclasses typeClassDependencies typeClassIsEmpty
      ]
  toExternsDeclaration (TypeInstanceRef ss' ident ns)
    = [ EDInstance tcdClassName (lookupRenamedIdent ident) tcdForAll tcdInstanceKinds tcdInstanceTypes tcdDependencies tcdChain tcdIndex ns ss'
      | m1 <- maybeToList (M.lookup (ByModuleName mn) (typeClassDictionaries env))
      , m2 <- M.elems m1
      , nel <- maybeToList (M.lookup (Qualified (ByModuleName mn) ident) m2)
      , TypeClassDictionaryInScope{..} <- NEL.toList nel
      ]
  toExternsDeclaration _ = []

  renameRef :: DeclarationRef -> DeclarationRef
  renameRef = \case
    ValueRef ss' ident -> ValueRef ss' $ lookupRenamedIdent ident
    TypeInstanceRef ss' ident _ | not $ isPlainIdent ident -> TypeInstanceRef ss' (lookupRenamedIdent ident) CompilerNamed
    other -> other

  lookupRenamedIdent :: Ident -> Ident
  lookupRenamedIdent = flip (join M.findWithDefault) renamedIdents

externsFileName :: FilePath
externsFileName = "externs.bin"

moduIsPrim :: ModuleName -> Bool
moduIsPrim (ModuleName n) = "Prim" `T.isPrefixOf` n
