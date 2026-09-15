module Language.PureScript.Make.Actions
  ( MakeActions(..)
  , RebuildPolicy(..)
  , ProgressMessage(..)
  , renderProgressMessage
  , buildMakeActions
  , cacheDbFile
  , readCacheDb'
  , writeCacheDb'
  , ffiCodegen'
  , RecompileReason(..)
  ) where

import Prelude

import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (asks)
import Control.Monad.Supply (SupplyT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime)
import Data.Version (showVersion)
import Language.PureScript.CoreFn qualified as CF
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), errorMessage, errorMessage')
import Language.PureScript.Externs (ExternsFile, externsFileName)
-- import Language.PureScript.Make.Monad (Make, copyFile, getTimestamp, getTimestampMaybe, hashFile, makeIO, readExternsFile, readJSONFile, readTextFile, writeCborFile, writeJSONFile, writeTextFile)
import Language.PureScript.Make.Monad
import Language.PureScript.Make.Cache (CacheDb, ContentHash, normaliseForCache)
import Language.PureScript.Names (ModuleName, runModuleName)
import Language.PureScript.Options (CodegenTarget(..), Options(..))
import Paths_purescript qualified as Paths
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (stderr)
-- purerl
import Language.PureScript.Erl.CodeGen (buildCodegenEnvironment)
import Language.PureScript.AST as P
import Language.PureScript.Comments as P
import Language.PureScript.Crash as P
import Language.PureScript.Environment as P
import Language.PureScript.Errors as P hiding (indent)
import Language.PureScript.Externs as P
import Language.PureScript.Linter as P
import Language.PureScript.ModuleDependencies as P
import Language.PureScript.Names as P
import Language.PureScript.Options as P
import Language.PureScript.Pretty as P
import Language.PureScript.Renamer as P
import Language.PureScript.Roles as P
import Language.PureScript.Sugar as P
import Language.PureScript.TypeChecker as P
import Language.PureScript.Types as P
import Language.PureScript.Erl as Purserl

import Data.Maybe (catMaybes)

import qualified Build as Erl.Build

import Data.Either (fromRight)
import           Language.PureScript.Erl.Parser (parseFile)
import           Data.List ((\\))
import           Language.PureScript.Erl.CodeGen (moduleToErl, CodegenEnvironment)
import           Language.PureScript.Erl.CodeGen.Optimizer (optimize)
import           Language.PureScript.Erl.Pretty (prettyPrintErl)
import           Language.PureScript.Erl.CodeGen.Common (erlModuleName, erlModuleNameBase, atomModuleName, atom, ModuleType(..), runAtom)

import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Supply as SupplyT
import qualified Control.Monad.Logger as Logger
import qualified Language.PureScript.Erl.Errors
import qualified Language.PureScript.Erl.Make.Monad


--

import Data.IORef as IORef

import Debug.Trace
import System.IO.Unsafe

-- | Determines when to rebuild a module
data RebuildPolicy
  -- | Never rebuild this module
  = RebuildNever
  -- | Always rebuild this module
  | RebuildAlways
  deriving (Show, Eq, Ord)

-- | Progress messages from the make process
data ProgressMessage
  -- = CompilingModule ModuleName (Maybe (Int, Int)) String
  = CompilingModule ModuleName (Maybe (Int, Int)) RecompileReason
  -- ^ Compilation started for the specified module
  | CompileMeta T.Text
  -- ^ [drathier]: Various stuff we want to print to describe to purerlex where we are in the compilation flow
  deriving (Show, Eq, Ord)

data RecompileReason
  = UnknownRecompileReason
  | SourceChangedOrDependencyFailedToBuildInPreviousCompilationOrSomethingElse
  | DependencyChanged ModuleName
  deriving (Show, Eq, Ord)

-- | Render a progress message
renderProgressMessage :: T.Text -> ProgressMessage -> T.Text
--renderProgressMessage infx (CompilingModule mn mi ms) =
renderProgressMessage infx (CompileMeta t) = t
renderProgressMessage infx (CompilingModule mn mi causedByModule) =
  T.concat $
    [ renderProgressIndex mi
    -- , " "
    -- , T.pack (show ms)
    -- , " "
    , infx
    , runModuleName mn
    ]
    <>
    case causedByModule of
      DependencyChanged causeModule -> [" (", runModuleName causeModule, " changed)"]
      SourceChangedOrDependencyFailedToBuildInPreviousCompilationOrSomethingElse -> []
      UnknownRecompileReason -> []
  where
  renderProgressIndex :: Maybe (Int, Int) -> T.Text
  renderProgressIndex = maybe "" $ \(start, end) ->
    let start' = T.pack (show start)
        end' = T.pack (show end)
        preSpace = T.replicate (T.length end' - T.length start') " "
    in "[" <> preSpace <> start' <> " of " <> end' <> "] "

-- | Actions that require implementations when running in "make" mode.
--
-- This type exists to make two things abstract:
--
-- * The particular backend being used (JavaScript, C++11, etc.)
--
-- * The details of how files are read/written etc.
data MakeActions m = MakeActions
  { getInputTimestampsAndHashes :: ModuleName -> m (Either RebuildPolicy (M.Map FilePath (UTCTime, m ContentHash)))
  -- ^ Get the timestamps and content hashes for the input files for a module.
  -- The content hash is returned as a monadic action so that the file does not
  -- have to be read if it's not necessary.
  , getOutputTimestamp :: ModuleName -> m (Maybe UTCTime)
  -- ^ Get the time this module was last compiled, provided that all of the
  -- requested codegen targets were also produced then. The defaultMakeActions
  -- implementation uses the modification time of the externs file, because the
  -- externs file is written first and we always write one. If there is no
  -- externs file, or if any of the requested codegen targets were not produced
  -- the last time this module was compiled, this function must return Nothing;
  -- this indicates that the module will have to be recompiled.
  , touchOutputTimestamp :: ModuleName -> m (Maybe ())
  -- ^ Set the time this module was last compiled to the current time. Similar
  -- to and used with getOutputTimestamp.
  , readExterns :: ModuleName -> m (FilePath, Maybe ExternsFile)
  -- ^ Read the externs file for a module as a string and also return the actual
  -- path for the file.
  , codegen :: Environment -> CF.Module CF.Ann -> ExternsFile -> SupplyT m ()
  -- ^ Run the code generator for the module and write any required output files.
  , ffiCodegen :: CF.Module CF.Ann -> m ()
  -- ^ Check ffi and print it in the output directory.
  , progress :: ProgressMessage -> m ()
  -- ^ Respond to a progress update.
  , readCacheDb :: m CacheDb
  -- ^ Read the cache database (which contains timestamps and hashes for input
  -- files) from some external source, e.g. a file on disk.
  , writeCacheDb :: CacheDb -> m ()
  -- ^ Write the given cache database to some external source (e.g. a file on
  -- disk).
  }

-- | Given the output directory, determines the location for the
-- CacheDb file
--
-- This is CBOR, not JSON, so that reading/writing the whole project's cache
-- database on every build doesn't pay JSON parse/encode costs (it reuses the
-- same 'Serialise' machinery already used for externs files). Old
-- `cache-db.json` files from a previous version are simply never read by
-- this filename and are treated as a cache miss.
cacheDbFile :: FilePath -> FilePath
cacheDbFile = (</> "cache-db.cbor")

readCacheDb'
  :: (MonadIO m, MonadError MultipleErrors m)
  => FilePath
  -- ^ The path to the output directory
  -> m CacheDb
readCacheDb' outputDir =
  fromMaybe mempty <$> readCborFile (cacheDbFile outputDir)

writeCacheDb'
  :: (MonadIO m, MonadError MultipleErrors m)
  => FilePath
  -- ^ The path to the output directory
  -> CacheDb
  -- ^ The CacheDb to be written
  -> m ()
writeCacheDb' outputDir cacheDb =
  makeIO ("write Cbor file: " <> T.pack (cacheDbFile outputDir)) (writeCborFileIO (cacheDbFile outputDir) cacheDb)

-- | A set of make actions that read and write modules from the given directory.
buildMakeActions
  :: FilePath
  -- ^ the output directory
  -> M.Map ModuleName (Either RebuildPolicy FilePath)
  -- ^ a map between module names and paths to the file containing the PureScript module
  -> M.Map ModuleName FilePath
  -- ^ a map between module name and the file containing the foreign javascript for the module
  -> Bool
  -- ^ Generate a prefix comment?
  -> Maybe ExternsMemCache
  -- ^ Optional memcache of already parsed externs files, for repeated builds
  -> MakeActions Make
buildMakeActions outputDir filePathMap foreigns usePrefix mExternsMemCache =
    MakeActions getInputTimestampsAndHashes getOutputTimestamp touchOutputTimestamp readExterns codegen ffiCodegen progress readCacheDb writeCacheDb
  where

  getInputTimestampsAndHashes
    :: ModuleName
    -> Make (Either RebuildPolicy (M.Map FilePath (UTCTime, Make ContentHash)))
  getInputTimestampsAndHashes mn = do
    -- TODO[drathier]: split timestamp and hash generation into two steps, so we don't have to open unchanged files?
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    case path of
      Left policy ->
        return (Left policy)
      Right filePath -> do
        cwd <- makeIO "Getting the current directory" getCurrentDirectory
        let inputPaths = map (normaliseForCache cwd) (filePath : maybeToList (M.lookup mn foreigns))
            getInfo fp = do
              ts <- getTimestamp fp
              return (ts, hashFile fp)
        pathsWithInfo <- traverse (\fp -> (fp,) <$> getInfo fp) inputPaths
        return $ Right $ M.fromList pathsWithInfo

  outputFilename :: ModuleName -> String -> FilePath
  outputFilename mn fn =
    let filePath = T.unpack (runModuleName mn)
    in outputDir </> filePath </> fn

  targetFilename :: ModuleName -> CodegenTarget -> FilePath
  targetFilename mn = \case
    Erl -> outFile mn

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    codegenTargets <- asks optionsCodegenTargets
    mExternsTimestamp <- getTimestampMaybe (outputFilename mn externsFileName)
    case mExternsTimestamp of
      Nothing ->
        -- If there is no externs file, we will need to compile the module in
        -- order to produce one.
        pure Nothing
      Just externsTimestamp ->
        case NEL.nonEmpty (fmap (targetFilename mn) (S.toList codegenTargets)) of
          Nothing ->
            -- If the externs file exists and no other codegen targets have
            -- been requested, then we can consider the module up-to-date
            pure (Just externsTimestamp)
          Just outputPaths -> do
            -- If any of the other output paths are nonexistent or older than
            -- the externs file, then they should be considered outdated, and
            -- so the module will need rebuilding.
            mmodTimes <- traverse getTimestampMaybe outputPaths
            pure $ case sequence mmodTimes of
              Nothing ->
                Nothing
              Just modTimes ->
                if externsTimestamp <= minimum modTimes
                  then Just externsTimestamp
                  else Nothing

  touchOutputTimestamp :: ModuleName -> Make (Maybe ())
  touchOutputTimestamp mn = do
    -- first check that all relevant files exist, by fetching the timestamp of the existing cache files
    externsTimestamp <- getOutputTimestamp mn
    -- then touch them all, to mark them as up-to-date. If this fails partway through, the next getOutputTimestamp will consider it incomplete.
    codegenTargets <- asks optionsCodegenTargets
    case externsTimestamp of
      Nothing -> pure Nothing
      Just _ -> do
        -- then, after reading all relevant files succeeded, we update their mtimes
        touchTimestampMaybe (outputFilename mn externsFileName)
        case NEL.nonEmpty (fmap (targetFilename mn) (S.toList codegenTargets)) of
          Nothing ->
            pure (Just ())
          Just outputPaths -> do
            mmodTimes <- traverse touchTimestampMaybe outputPaths
            pure $ sequence_ mmodTimes


  readExterns :: ModuleName -> Make (FilePath, Maybe ExternsFile)
  readExterns mn = do
    let path = outputDir </> T.unpack (runModuleName mn) </> externsFileName
    (path, ) <$> readExternsFile mExternsMemCache path

  -- purserl: computed once, lazily, on first demand, instead of once per
  -- codegen/ffiCodegen call. Each `Erl.Build.inferForeignModule'` call does a
  -- `doesFileExist` for one module; re-deriving this map for the whole
  -- project inside every module's codegen/ffiCodegen made a full build
  -- O(modules^2) in this lookup alone. It only depends on the `foreigns`
  -- argument (fixed for the lifetime of this `MakeActions`), so it's safe to
  -- share a single evaluation across every module.
  erlForeigns :: M.Map ModuleName FilePath
  erlForeigns = unsafePerformIO $ do
    (merls :: M.Map ModuleName (Maybe FilePath)) <- traverse Erl.Build.inferForeignModule' foreigns
    case sequence merls of
      Nothing -> internalError (show ("couldn't find some erl foreign", merls))
      Just v -> pure v
  {-# NOINLINE erlForeigns #-}

-- ########################

  moduleDir mn = outputDir </> T.unpack (P.runModuleName mn)
  outFile mn = moduleDir mn </> T.unpack (erlModuleName mn PureScriptModule) ++ ".erl"
  outFileChecked mn = moduleDir mn </> T.unpack (erlModuleName mn PureScriptCheckedModule) ++ ".erl"
  hrlFile mn = moduleDir mn </> T.unpack (erlModuleNameBase mn) ++ ".hrl"
  foreignHrlFile mn = moduleDir mn </> T.unpack (erlModuleName mn ForeignModule) ++ ".hrl"



  codegen :: Environment -> CF.Module CF.Ann -> ExternsFile -> SupplyT Make ()
  codegen environment m exts = do
    let mn = CF.moduleName m
    lift $ writeCborFile mExternsMemCache (outputFilename mn externsFileName) exts
    codegenTargets <- lift $ asks optionsCodegenTargets

    -- ### Purerl

    when (S.member Erl codegenTargets) $ do
      -- generate the corefn
      -- let coreFnFile = targetFilename mn CoreFn
      --     json = CFJ.moduleToJSON Paths.version m
      -- lift $ writeJSONFile coreFnFile json

      let externsFiles = exts

      let  getForeigns :: String -> Make [(T.Text, Int)]
           getForeigns path = do
             -- liftIO $ putStrLn (show ("getForeigns", path))
             text <- readTextFile path
             let (exports, ignoreExports) = fromRight ([],[]) $ parseFile path text
             pure $ exports \\ ignoreExports


      -- purerl env
      -- let env = buildCodegenEnvironment $ foldr P.applyExternsFileToEnvironment P.initEnvironment (catMaybes externsFiles)
      -- make sure our own externs are included in the environment
      let env = buildCodegenEnvironment (P.applyExternsFileToEnvironment exts environment)

      -- and generate the erlang code
        -- codegen :: CodegenEnvironment -> CF.Module CF.Ann -> SupplyT Make ()
        -- codegen env m = do
      let mn = CF.moduleName m
      foreignExports <- lift $ case mn `M.lookup` erlForeigns of
        Just path
          | not $ requiresForeign m ->
              return []
          | otherwise ->
              getForeigns path
        Nothing ->
          return []

      (exports, typeDecls, foreignSpecs, rawErl, checkedExports, checkedRawErl) <- do
        -- SupplyT.mapSupplyT
        --   (\(Language.PureScript.Erl.Make.Monad.Make m, i) ->
        --     (Make
        --       -- ExceptT
        --       $ ReaderT.mapReaderT
        --          (ExceptT.withExceptT
        --            (\(Language.PureScript.Erl.Errors.MultipleErrors errors) -> MultipleErrors [])
        --          )
        --       -- Logger
        --       $ ReaderT.mapReaderT
        --          (ExceptT.mapExceptT
        --            (Logger.contraMapLoggerErrors
        --               (\(MultipleErrors _) -> Language.PureScript.Erl.Errors.MultipleErrors [])
        --            )
        --          )
        --        m
        --     , i)
        --   )

        let f m =
              liftIO $ do
                (l, r) <-
                  Language.PureScript.Erl.Make.Monad.runMake
                    (Options False False codegenTargets)
                    m

                case (l, r) of
                  -- TODO[drathier]: don't throw away purerl errors and warnings
                  (Right a, _) -> pure a
                  (Left lerrs, rerrs) -> internalError (show (Language.PureScript.Erl.Errors.runMultipleErrors lerrs, rerrs))

        SupplyT.mapSupplyT
          f
          (moduleToErl env m foreignExports) -- :: SupplyT Language.PureScript.Erl.Make.Monad.Make

{-
      !_ <-
        case True of -- runModuleName mn == "Hex" of
          True -> pure $ unsafePerformIO $ writeFile ("ast/" <> T.unpack (runModuleName mn) <> ".txt") (T.unpack $ T.replace "EFunctionDef" "\nEFunctionDef" $ T.pack $ show rawErl)
          False -> pure ()
      !_ <-
        case True of -- runModuleName mn == "Hex" of
          True -> pure $ unsafePerformIO $ writeFile ("ast/" <> T.unpack (runModuleName mn) <> ".corefn.txt") (T.unpack $ T.replace ",Rec" ",\nRec" $ T.replace ",NonRec" ",\nNonRec" $ T.pack $ show m)
          False -> pure ()
-}
      optimized <- optimize exports rawErl
      checked <- optimize checkedExports checkedRawErl
{-
      !_ <-
        case True of -- runModuleName mn == "Hex" of
          True -> pure $ unsafePerformIO $ writeFile ("ast/" <> T.unpack (runModuleName mn) <> ".erlopt.txt") (T.unpack $ T.replace "EFunctionDef" "\nEFunctionDef" $ T.pack $ show optimized)
          False -> pure ()
-}
      dir <- lift $ makeIO "get file info: ." getCurrentDirectory
      let makeAbsFile file = dir </> file
      let pretty = prettyPrintErl makeAbsFile optimized
          -- prettyChecked = prettyPrintErl makeAbsFile checked
          -- prettySpecs = prettyPrintErl makeAbsFile foreignSpecs
          -- prettyDecls = prettyPrintErl makeAbsFile typeDecls

      let
          prefix :: [T.Text]
          prefix = ["Generated by purerl version nadaopt " <> T.pack (showVersion Paths.version) | usePrefix]
          -- directives :: [(T.Text, Int)] -> ModuleType -> [T.Text]
          directives exports' moduleType = [
            "-module(" <> atom (atomModuleName mn moduleType) <> ").",
            "-export([" <> T.intercalate ", " (map (\(f, a) -> runAtom f <> "/" <> T.pack (show a)) exports') <> "]).",
            "-compile(nowarn_shadow_vars).",
            "-compile(nowarn_unused_vars).",
            "-compile(nowarn_export_all).",
            "-compile(nowarn_ignored).",
            "-compile(nowarn_nomatch).",
            "-compile(nowarn_obsolete_guard).",
            "-compile(nowarn_opportunistic).",
            "-compile(nowarn_unused_function).",
            "-compile(no_auto_import)."
            -- includeHrl,
            -- "-ifndef(PURERL_MEMOIZE).",
            -- "-define(MEMOIZE(X), X).",
            -- "-else.",
            -- "-define(MEMOIZE, memoize).",
            -- "memoize(X) -> X.",
            -- "-endif."
            ]
          -- includeHrl :: T.Text
          -- includeHrl = "-include(\"./" <> erlModuleNameBase mn <> ".hrl\").\n"
      let erl :: T.Text = T.unlines $ map ("% " <>) prefix ++ directives exports PureScriptModule ++  [ pretty ]
      lift $ writeTextFile (outFile mn) $ TE.encodeUtf8 erl

      -- when generateChecked $ do
      --   let erlchecked :: T.Text = T.unlines $ map ("% " <>) prefix ++ directives checkedExports PureScriptCheckedModule ++  [ prettyChecked ]
      --   lift $ writeTextFile (outFileChecked mn) $ TE.encodeUtf8 erlchecked

      -- let hrl :: T.Text = T.unlines $ map ("% " <>) prefix ++ [ prettyDecls ]
      -- lift $ writeTextFile (hrlFile mn) $ TE.encodeUtf8 hrl

      -- let foreignHrl :: T.Text = T.unlines $ map ("% " <>) prefix ++ [ includeHrl, prettySpecs ]
      -- lift $ writeTextFile (foreignHrlFile mn) $ TE.encodeUtf8 foreignHrl

  ffiCodegen :: CF.Module CF.Ann -> Make ()
  ffiCodegen m = do
    codegenTargets <- asks optionsCodegenTargets

    -- purserl, inlined because we need values from closure
    when (S.member Erl codegenTargets) $ do
          let mn = CF.moduleName m
              foreignFile = moduleDir mn </> T.unpack (erlModuleName mn ForeignModule) ++ ".erl"
          case mn `M.lookup` erlForeigns of
            Just path
              | not $ requiresForeign m ->
                  tell $ errorMessage $ UnnecessaryFFIModule mn path
              | otherwise -> pure ()
            Nothing -> do
              when (requiresForeign m) $ liftIO $ putStrLn (show ("PossiblyMissingFFIModule", "required", requiresForeign m, "mn", mn, "foreignFile", foreignFile, "modules", M.keys erlForeigns))
              when (requiresForeign m) $ throwError . errorMessage $ MissingFFIModule mn
          for_ (mn `M.lookup` erlForeigns) $ \path ->
            copyFile path foreignFile


  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  progress :: ProgressMessage -> Make ()
  progress = liftIO . TIO.hPutStr stderr . (<> "\n") . renderProgressMessage ("Compiling " <> Purserl.versionString <> " ")

  readCacheDb :: Make CacheDb
  readCacheDb = readCacheDb' outputDir

  writeCacheDb :: CacheDb -> Make ()
  writeCacheDb = writeCacheDb' outputDir

-- | Checks that a module has a foreign file if and only if it needs one, and
-- (if a path maker is supplied) copies the foreign file to the output
-- directory. Used by the IDE's "pure" (type-check-only) rebuild path, via
-- 'Language.PureScript.Ide.Rebuild.enableForeignCheck', to catch a missing or
-- unnecessary foreign module without running full codegen. (The real Erlang
-- FFI check/copy for a normal build lives inline in 'buildMakeActions'’s own
-- 'ffiCodegen' above, since it needs values from that closure.)
ffiCodegen'
  :: M.Map ModuleName FilePath
  -> Maybe (ModuleName -> String -> FilePath)
  -> CF.Module CF.Ann
  -> Make ()
ffiCodegen' foreigns makeOutputPath m =
  case mn `M.lookup` foreigns of
    Just path
      | not $ requiresForeign m ->
          tell $ errorMessage' (CF.moduleSourceSpan m) $ UnnecessaryFFIModule mn path
      | otherwise ->
          for_ makeOutputPath (\outputFilename -> copyFile path (outputFilename mn "foreign.erl"))
    Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
            | otherwise -> return ()
  where
  mn = CF.moduleName m
  requiresForeign = not . null . CF.moduleForeign
