module Language.PureScript.Make.BuildPlan
  ( BuildPlan(bpEnv, bpIndex)
  , BuildJobResult(..)
  , ResultTiming(..)
  , bpExterns
  , construct2
  , getExternFromLastSuccessfulPreviousBuild
  , needsRebuildEvenAfterDiffingCacheShapes
  , CacheShapeDiffResult(..)
  , anyDepChanged
  , RebuildInstructions(..)
  , getResult
  , fetchMissingExtern
  , fetchMissingExterns
  , collectResults
  , markComplete
  , markComplete2
  , markExternsComplete
  , markCompleteImmediate
  , needsRebuild
  ) where

import Prelude

import Codec.Serialise (serialise)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async.Lifted as A
import Control.Concurrent.Lifted as C
import Control.Exception.Lifted (onException)
import Control.Monad.Base (liftBase)
-- import Control.Monad (foldM)
import Control.Monad
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Language.PureScript.AST (Module, getModuleName)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors (MultipleErrors(..))
-- import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Externs
import qualified Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.Actions (MakeActions(..), RebuildPolicy(..), ProgressMessage(..))
import Language.PureScript.Make.Cache (CacheDb, CacheInfo, checkChanged)
import Language.PureScript.Names (ModuleName, runModuleName)
import Language.PureScript.Sugar.Names.Env (Env, primEnv)
import System.Directory (getCurrentDirectory)
import qualified Data.Text as T
import Debug.Trace
import PrettyPrint
import Data.Foldable

scratchpad = do
  -- did any dep input file hashes change?
  -- if so, did their hashes change?
  -- inputInfo <- getInputTimestampsAndHashes moduleName
  -- cacheChanged <- A.forConcurrently sortedModuleNames getRebuildStatusIsUpToDate
  Just 42

-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  , bpDirtyExterns :: M.Map ModuleName CacheFilesAvailable
  , bpBuildJobs :: M.Map ModuleName BuildJob
  , bpEnv :: C.MVar Env
  , bpIndex :: C.MVar Int
  -- todo[drathier]: mvar map of mvars is slow
  , bpCacheResult :: M.Map ModuleName (MVar (Maybe CacheResult))
  , bpExterns :: M.Map ModuleName (MVar (Maybe ExternsFile))
  }

data CacheResult
  = NoExternsChange
  | ExternsChanged
  deriving(Show)

data RebuildInstructions
  = DepsChangedPleaseRebuildIfNeeded ModuleName
  | FailRebuildDepsFailed ModuleName
  | FullDepsCacheHit
  deriving(Show)

data Prebuilt = Prebuilt
  { pbModificationTime :: UTCTime
  , pbExternsFile :: ExternsFile
  }
  deriving (Show)

data BuildJob = BuildJob
  { bjExterns :: C.MVar BuildJobResult
    -- ^ Filled as soon as this module's ExternsFile is known (i.e. right
    -- after typechecking finishes), independently of whether/when this
    -- module's codegen has finished. Other modules that only need our
    -- ExternsFile to proceed with their own typechecking should block on
    -- this MVar rather than 'bjResult', so they don't needlessly wait on our
    -- codegen. Note: an empty MVar indicates that typechecking has not yet
    -- finished (or won't -- e.g. this module was skipped).
  , bjResult :: C.MVar BuildJobResult
    -- ^ Filled once this module's entire build (typecheck *and* codegen) has
    -- finished. Note: an empty MVar indicates that the build job has not yet
    -- finished.
  }

data BuildJobResult
  = BuildJobSucceeded !MultipleErrors !ExternsFile
  -- ^ Succeeded, with warnings and externs
  --
  | BuildJobFailed !MultipleErrors
  -- ^ Failed, with errors

  | BuildJobSkipped
  -- ^ The build job was not run, because an upstream build job failed

  | BuildJobSkippedFullCacheHit
  -- ^ The build job was not run, because no upstream files changed

-- | Called when we finished compiling a module (both typecheck *and*
-- codegen) and want to report back the final compilation result, as well as
-- any potential errors that were thrown. This only fills 'bjResult' -- see
-- 'markExternsComplete' for publishing the ExternsFile to dependents earlier,
-- right after typechecking.
markComplete
  :: (MonadBaseControl IO m)
  => MakeActions m
  -> BuildPlan
  -> ModuleName
  -> BuildJobResult
  -> m ()
markComplete _ma buildPlan moduleName result = do
  liftBase $ case result of
      BuildJobSucceeded _ _ ->
        putStrLn $ "### CS.BuildJobSucceeded[" <> T.unpack (runModuleName moduleName) <> "]"
      BuildJobFailed _ ->
        putStrLn $ "### CS.BuildJobFailed[" <> T.unpack (runModuleName moduleName) <> "]"
      BuildJobSkipped ->
        putStrLn $ "### CS.BuildJobSkipped[" <> T.unpack (runModuleName moduleName) <> "]"
      BuildJobSkippedFullCacheHit ->
        -- putStrLn $ "### CS.BuildJobSkippedFullCacheHit[" <> T.unpack (runModuleName moduleName) <> "]"
        pure ()
  let BuildJob { bjResult = rVar } = fromMaybe (internalError "make: markComplete no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
  putMVar rVar result

-- | Called as soon as a module's ExternsFile is known (right after
-- typechecking finishes, before codegen runs). Fills 'bjExterns' so that
-- dependent modules blocked on our externs (via 'getResult' with 'Early')
-- can proceed without waiting for our codegen, and updates 'bpCacheResult'
-- (via 'markComplete2') since that decision is likewise purely a function of
-- typecheck output.
markExternsComplete
  :: (MonadBaseControl IO m)
  => MakeActions m
  -> BuildPlan
  -> ModuleName
  -> Maybe ExternsFile
  -> BuildJobResult
  -> m ()
markExternsComplete ma buildPlan moduleName oldExt result = do
  let BuildJob { bjExterns = eVar } = fromMaybe (internalError "make: markExternsComplete no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
  putMVar eVar result

  markComplete2 ma buildPlan moduleName oldExt result

-- | Convenience wrapper for the skip/fail paths that have no typecheck/codegen
-- split of their own (the module was never actually built this run) --
-- publishes the same result to both 'bjExterns' and 'bjResult' (plus
-- 'bpCacheResult') immediately.
markCompleteImmediate
  :: (MonadBaseControl IO m)
  => MakeActions m
  -> BuildPlan
  -> ModuleName
  -> Maybe ExternsFile
  -> BuildJobResult
  -> m ()
markCompleteImmediate ma buildPlan moduleName oldExt result = do
  markExternsComplete ma buildPlan moduleName oldExt result
  markComplete ma buildPlan moduleName result


-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete2
  :: (MonadBaseControl IO m)
  => MakeActions m
  -> BuildPlan
  -> ModuleName
  -> Maybe ExternsFile
  -> BuildJobResult
  -> m ()
markComplete2 ma@MakeActions{..} buildPlan moduleName oldExt result = do
--  liftBase $ putStrLn $ case result of
--      BuildJobSucceeded _ _ ->
--        "### CS.BuildJobSucceeded[" <> T.unpack (runModuleName moduleName) <> "]"
--      BuildJobFailed _ ->
--        "### CS.BuildJobFailed[" <> T.unpack (runModuleName moduleName) <> "]"
--      BuildJobSkipped ->
--        "### CS.BuildJobSkipped[" <> T.unpack (runModuleName moduleName) <> "]"

  --(_,oldExt) <- fetchMissingExtern () ma buildPlan moduleName
  let cfa = getCacheFilesAvailable buildPlan moduleName
--  (case result of
--    BuildJobFailed _ -> pure ()
--    BuildJobSkipped -> pure ()
--    BuildJobSkippedFullCacheHit -> pure ()
--    BuildJobSucceeded _ newExt -> do
--      progress $ CompileMeta (T.pack $ show ("-- BP.externsDiff[" <> runModuleName moduleName <> "]", ("eq?", Just newExt == oldExt), ("serialise-eq?", Just (serialise newExt) == fmap serialise oldExt), ("serialise-opaque-eq?", serialiseDbEq newExt oldExt)))
--      progress $ CompileMeta (T.pack $ show ("-- BP.externsDiff[" <> runModuleName moduleName <> "]New", Just newExt))
--      progress $ CompileMeta (T.pack $ show ("-- BP.externsDiff[" <> runModuleName moduleName <> "]Old", oldExt))
--
--      pure ()
--    )
  putMVar
    (fromMaybe (internalError (show ("BuildPlan: bpCacheResult mvar not found for module", moduleName))) $ M.lookup moduleName (bpCacheResult buildPlan))

    (case result of
      BuildJobFailed _ -> Nothing
      BuildJobSkipped -> Nothing
      BuildJobSkippedFullCacheHit -> Just NoExternsChange
      BuildJobSucceeded _ newExt -> do
        case fmap (serialise . efOurCacheShapes) oldExt == Just (serialise $ efOurCacheShapes newExt) of
          True ->
            Just NoExternsChange
          False ->
            Just ExternsChanged
    )

serialiseDbEq (ExternsFile efVersion1 efModuleName1 efExports1 efImports1 efFixities1 efTypeFixities1 efDeclarations1 efSourceSpan1 efUpstreamCacheShapes1 efOurCacheShapes1) mb =
  case mb of
    Nothing -> []
    Just (ExternsFile efVersion2 efModuleName2 efExports2 efImports2 efFixities2 efTypeFixities2 efDeclarations2 efSourceSpan2 efUpstreamCacheShapes2 efOurCacheShapes2) ->
      filter
      (\(x, y) -> y == False)
      [ ("efVersion", serialise efVersion1 == serialise efVersion2)
      , ("efModuleName", serialise efModuleName1 == serialise efModuleName2)
      , ("efExports", serialise efExports1 == serialise efExports2)
      , ("efImports", serialise efImports1 == serialise efImports2)
      , ("efFixities", serialise efFixities1 == serialise efFixities2)
      , ("efTypeFixities", serialise efTypeFixities1 == serialise efTypeFixities2)
      , ("efDeclarations", serialise efDeclarations1 == serialise efDeclarations2)
      , ("efSourceSpan", serialise efSourceSpan1 == serialise efSourceSpan2)
      , ("efUpstreamCacheShapes", serialise efUpstreamCacheShapes1 == serialise efUpstreamCacheShapes2)
      , ("efOurCacheShapes", serialise efOurCacheShapes1 == serialise efOurCacheShapes2)
      ]

-- | Whether or not the module with the given ModuleName needs to be rebuilt
needsRebuild :: BuildPlan -> ModuleName -> Bool
needsRebuild bp moduleName = M.member moduleName (bpBuildJobs bp)

-- | Collects results for all prebuilt as well as rebuilt modules. This will
-- block until all build jobs are finished. Prebuilt modules always return no
-- warnings.
collectResults
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> m (M.Map ModuleName BuildJobResult)
collectResults buildPlan = do
  let prebuiltResults = M.map (BuildJobSucceeded (MultipleErrors []) . pbExternsFile) (bpPrebuilt buildPlan)
  barrierResults <- traverse (readMVar . bjResult) $ bpBuildJobs buildPlan
  pure (M.union prebuiltResults barrierResults)

-- | Whether to read a module's build result as soon as its ExternsFile is
-- known ('Early', right after typechecking -- use this when another module
-- only needs our externs to proceed with its own typechecking) or only once
-- its entire build including codegen has finished ('Final' -- use this for
-- whole-build accounting, e.g. 'collectResults').
data ResultTiming = Early | Final

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getResult
  :: (MonadBaseControl IO m)
  => ResultTiming
  -> BuildPlan
  -> ModuleName
  -> m BuildJobResult
getResult timing buildPlan moduleName = do
  case M.lookup moduleName (bpPrebuilt buildPlan) of
    Just es ->
      pure (BuildJobSucceeded (MultipleErrors []) (pbExternsFile es))
    Nothing -> do
      let BuildJob { bjExterns, bjResult } = fromMaybe (internalError "make: no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
      readMVar $ case timing of
        Early -> bjExterns
        Final -> bjResult

fetchMissingExtern :: Show meta => MonadBaseControl IO m => ResultTiming -> meta -> MakeActions m -> BuildPlan -> ModuleName -> m BuildJobResult
fetchMissingExtern timing meta MakeActions{..} buildPlan moduleName = do
  mExts <- getResult timing buildPlan moduleName
  case mExts of
    BuildJobSucceeded warns v -> pure mExts
    BuildJobFailed err -> pure mExts
    -- TODO[drathier]: perhaps put BuildJobSkippedFullCacheHit externs into bjResult? Optional externs field?
    BuildJobSkipped -> pure mExts
    BuildJobSkippedFullCacheHit -> do
      let mvar = fromMaybe (internalError "BuildPlan: fetchMissingExtern") $ M.lookup moduleName (bpExterns buildPlan)
      e <- readMVar mvar
      -- read mvar, it's probably already filled and we don't want to interrupt anyone
      case e of
        -- Maybe wrapped value instead of tryReadMVar so that we don't have two threads decoding the same externs file, wasting work
        Just ext -> pure (BuildJobSucceeded (MultipleErrors []) ext)
        Nothing -> do
          -- oops, better fill in the mvar
          mv <- takeMVar mvar
          case mv of
            -- nope, someone did it before us
            Just extern -> do
              putMVar mvar mv
              pure (BuildJobSucceeded (MultipleErrors []) extern)

            Nothing -> do
              -- fill it in
              -- NOTE: we've already taken `mvar` above, emptying it. If anything
              -- below throws (e.g. the dependency genuinely failed to produce
              -- externs, so `readExterns`/the internalError below fires), we
              -- must restore it before rethrowing -- otherwise any other thread
              -- reading this same mvar would block on it forever.
              (do
                mextern <- snd <$> readExterns moduleName
                let extern = fromMaybe (internalError (show ("BuildPlan readExterns", moduleName, meta))) mextern
                putMVar mvar (Just extern)
                pure (BuildJobSucceeded (MultipleErrors []) extern)
                ) `onException` putMVar mvar Nothing

fetchMissingExterns :: Show meta => MonadBaseControl IO m => ResultTiming -> meta -> MakeActions m -> BuildPlan -> [ModuleName] -> m (M.Map ModuleName BuildJobResult)
fetchMissingExterns timing meta ma buildPlan deps =
  M.fromList <$> traverse (\dep -> (dep,) <$> fetchMissingExtern timing meta ma buildPlan dep) deps

data CacheShapeDiffResult
  = PleaseRebuild [(ModuleName, DBOpaque)]
  | NoRebuildNeeded

needsRebuildEvenAfterDiffingCacheShapes Nothing upstream = PleaseRebuild []
needsRebuildEvenAfterDiffingCacheShapes (Just oldExts) upstream =
  let ourCachedUpstreamCacheShapes = efUpstreamCacheShapes oldExts in
  let relevantUpstreamModules = M.intersectionWith (\_ s -> efOurCacheShapes s) ourCachedUpstreamCacheShapes $ upstream in
  let moduleName = efModuleName oldExts in

  -- TODO[drathier]: only diff exports list if it's an unsafe import
  let res =
        M.merge
          (M.mapMissing (\k a -> [(k,a)])) -- upstream cache shape is no longer present; perhaps we switched branches? Rebuild needed nomatter what happened.
          (M.mapMissing (\k b -> [(k,b)]))
          (M.zipWithMatched (\k a b ->
            let x = dbOpaqueDiffDiffIgnoringExportsListChanges a b in
            if x == mempty
            then [] else [(k,x)]
          ))
          ourCachedUpstreamCacheShapes
          relevantUpstreamModules
  in case fold res of
    [] ->
      NoRebuildNeeded
    errs ->
      PleaseRebuild errs

data CacheFilesAvailable
  = DepChanged Prebuilt
  | SourceChanged
  | UpToDate Prebuilt

instance Show CacheFilesAvailable where
  show cfa =
    case cfa of
      UpToDate _ -> "UpToDate.."
      SourceChanged -> "SourceChanged"
      DepChanged _ -> "DepChanged.."

cfaPrebuilt :: CacheFilesAvailable -> Maybe Prebuilt
cfaPrebuilt cfa =
  case cfa of
    DepChanged pb -> Just pb
    SourceChanged -> Nothing
    UpToDate pb -> Just pb

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getCacheFilesAvailable
  :: BuildPlan
  -> ModuleName
  -> CacheFilesAvailable
getCacheFilesAvailable buildPlan moduleName =
  case M.lookup moduleName (bpDirtyExterns buildPlan) of
    Just v -> v
    Nothing -> SourceChanged

getExternFromLastSuccessfulPreviousBuild :: Monad m => MakeActions m -> BuildPlan -> ModuleName -> m (Maybe ExternsFile)
getExternFromLastSuccessfulPreviousBuild MakeActions{..} _buildPlan moduleName = do
  fmap snd $ readExterns moduleName

-- | Cheaply checks (timestamp/hash only, no externs reads) whether a
-- module's own source files are up to date according to the cache db.
-- Doesn't check dependencies -- used to decide, up front, whether every
-- module in the project is up to date so 'construct2' can take its fast
-- path.
getRebuildStatusIsUpToDate :: forall m. MonadBaseControl IO m => MakeActions m -> CacheDb -> ModuleName -> m Bool
getRebuildStatusIsUpToDate MakeActions{..} cacheDb moduleName = do
  inputInfo <- getInputTimestampsAndHashes moduleName
  case inputInfo of
    Left RebuildNever ->
      pure True
    Left RebuildAlways ->
      pure False
    Right cacheInfo -> do
      cwd <- liftBase getCurrentDirectory
      (_newCacheInfo, isUpToDate) <- checkChanged cacheDb moduleName cwd cacheInfo
      pure isUpToDate

anyDepChanged :: forall m. (MonadBaseControl IO m) => ModuleName -> [(ModuleName, [ModuleName])] -> BuildPlan -> m RebuildInstructions
anyDepChanged moduleName graph buildPlan = do
  let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
  let f things =
        case things of
          [] -> pure FullDepsCacheHit
          a:ax -> do
            let mvar =
                  fromMaybe (internalError (show ("BuildPlan: module not in deps", a, moduleName, deps)))
                    $ M.lookup a (bpCacheResult buildPlan)
            cacheResult <- readMVar mvar
            case cacheResult of
              Nothing -> pure (FailRebuildDepsFailed a)
              Just NoExternsChange -> f ax
              Just ExternsChanged -> pure (DepsChangedPleaseRebuildIfNeeded a)
  f deps

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
--
-- As a fast path, if every module in the project is already up to date, this
-- returns a BuildPlan with zero build jobs and 'bpPrebuilt' populated
-- directly from each module's on-disk externs -- so 'Make.make' doesn't fork
-- a thread per module, and doesn't have to re-read/re-decode every module's
-- externs file a second time at the end of the build, when nothing needed
-- rebuilding in the first place.
construct2
  :: forall m. MonadBaseControl IO m
  => MakeActions m
  -> CacheDb
  -> ([CST.PartialResult Module], [(ModuleName, [ModuleName])])
  -> m (BuildPlan, CacheDb)
construct2 ma@MakeActions{..} cacheDb (sorted, graph) = do
  let sortedModuleNames = map (getModuleName . CST.resPartial) sorted
  env <- C.newMVar primEnv
  idx <- C.newMVar 1
  cacheChanged <- A.forConcurrently sortedModuleNames (getRebuildStatusIsUpToDate ma cacheDb)
  case and cacheChanged of
    True -> do
      prebuiltOrMissing <- A.forConcurrently sortedModuleNames $ \mn -> do
        mts <- getOutputTimestamp mn
        (_, mexts) <- readExterns mn
        pure (mn, Prebuilt <$> mts <*> mexts)
      case traverse snd prebuiltOrMissing of
        Just pbs ->
          -- Genuinely nothing to do: every module is up to date and its
          -- externs/output are actually present on disk.
          pure
            ( BuildPlan (M.fromList (zip sortedModuleNames pbs)) M.empty M.empty env idx M.empty M.empty
            , cacheDb
            )
        Nothing ->
          -- The cache db says everything's up to date, but some module's
          -- externs/output is actually missing or stale on disk (e.g. output/
          -- was partially deleted). Fall back to rebuilding everything rather
          -- than trying to patch just the affected module(s).
          buildEverything sortedModuleNames env idx
    False -> buildEverything sortedModuleNames env idx
  where
    buildEverything sortedModuleNames env idx = do
      let makeBuildJob prev moduleName = do
            buildJob <- BuildJob <$> C.newEmptyMVar <*> C.newEmptyMVar
            pure (M.insert moduleName buildJob prev)
      buildJobs <- foldM makeBuildJob M.empty sortedModuleNames
      mapOfEmptyCacheResults <- foldM (\m mn -> (\v -> M.insert mn v m) <$> newEmptyMVar) M.empty sortedModuleNames
      mapOfEmptyExternResults <- foldM (\m mn -> (\v -> M.insert mn v m) <$> newMVar Nothing) M.empty sortedModuleNames
      pure
        ( BuildPlan M.empty M.empty buildJobs env idx mapOfEmptyCacheResults mapOfEmptyExternResults
        , cacheDb
        )
