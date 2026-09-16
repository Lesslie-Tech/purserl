module Language.PureScript.Make
  (
  -- * Make API
  rebuildModule
  , rebuildModule'
  , make
  , inferForeignModules
  , module Monad
  , module Actions
  ) where

import Prelude

import Control.Concurrent.Lifted as C
import Control.DeepSeq (force)
import Control.Exception.Lifted (onException, bracket_, evaluate)
import Control.Monad (foldM, unless, when, (<=<))
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Supply (evalSupplyT, runSupply, runSupplyT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad.Writer.Class (MonadWriter(..), censor)
import Control.Monad.Writer.Strict (runWriterT, lift)
import Data.Function (on)
import Data.Foldable (fold, for_)
import Data.List (foldl', sortOn)
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (fromMaybe)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.PureScript.AST (ErrorMessageHint(..), Module(..), SourceSpan(..), getModuleName, getModuleSourceSpan, importPrim)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Environment (Environment, initEnvironment)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), addHint, defaultPPEOptions, errorMessage', errorMessage'', prettyPrintMultipleErrors)
-- import Language.PureScript.Externs (ExternsFile, applyExternsFileToEnvironment, moduleToExternsFile)
import Language.PureScript.Externs
import Language.PureScript.Linter (Name(..), lint, lintImports)
import Language.PureScript.ModuleDependencies (DependencyDepth(..), moduleSignature, sortModules)
import Language.PureScript.Names (ModuleName, isBuiltinModuleName, runModuleName)
import Language.PureScript.Renamer (renameInModule)
import Language.PureScript.Sugar (Env, collapseBindingGroups, createBindingGroups, desugar, desugarCaseGuards, externsEnv, primEnv)
import Language.PureScript.TypeChecker (CheckState(..), emptyCheckState, typeCheckModule)
import Language.PureScript.Make.BuildPlan (BuildJobResult(..), BuildPlan(..))
import Language.PureScript.Make.BuildPlan qualified as BuildPlan
import Language.PureScript.Make.Cache qualified as Cache
import Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.Monad as Monad
import Language.PureScript.CoreFn qualified as CF
import Debug.Trace
import PrettyPrint
import Data.Text.IO qualified as T

-- purserl
import qualified Build as Erl.Build
import           System.Directory (getCurrentDirectory)
-- import System.IO.Unsafe (unsafePerformIO)
--


-- | Rebuild a single module.
--
-- This function is used for fast-rebuild workflows (PSCi and psc-ide are examples).
rebuildModule
  :: MakeActions Make
  -> [ExternsFile]
  -> Module
  -> Make ExternsFile
rebuildModule actions externs m = do
  env <- fmap fst . runWriterT $ foldM externsEnv primEnv externs
  rebuildModule' actions env externs m

rebuildModule'
  :: MakeActions Make
  -> Env
  -> [ExternsFile]
  -> Module
  -> Make ExternsFile
rebuildModule' act env ext mdl = rebuildModuleWithIndex act env ext mdl Nothing UnknownRecompileReason

-- | Everything phase B (codegen) needs, produced by phase A (typecheck).
data ModuleCheckResult = ModuleCheckResult
  { mcrUpstreamEnv :: Environment
    -- ^ The `Environment` built purely from upstream `externs` -- this is
    -- deliberately *not* `env'` (the post-typecheck environment including
    -- this module's own declarations); `codegen` only ever consumed the
    -- upstream one, even before this split.
  , mcrCheckedEnv :: Environment
    -- ^ `env'`, the post-typecheck environment including this module's own
    -- declarations -- only needed for docs conversion (phase B); NOT the
    -- same value `codegen` itself is given (see `mcrUpstreamEnv`).
  , mcrOriginalModule :: Module
  , mcrExternsInput :: [ExternsFile]
  , mcrExEnv :: Env
  , mcrRenamed :: CF.Module CF.Ann
  , mcrExterns :: ExternsFile
  , mcrNextVar :: Integer
  }

-- | Phase A of rebuilding a single module: parse (already done by the
-- caller) through typecheck, CoreFn generation/optimization, renaming, and
-- ffiCodegen -- i.e. everything needed to fully determine this module's
-- `ExternsFile`. Deliberately excludes the backend-specific `codegen` call
-- (phase B, see `rebuildModuleCodegen`), since that doesn't affect `exts` and
-- can safely be deferred so dependent modules aren't blocked on it.
rebuildModuleTypecheck
  :: MakeActions Make
  -> Env
  -> [ExternsFile]
  -> Module
  -> Maybe (Int, Int)
  -> RecompileReason
  -> Make ModuleCheckResult
rebuildModuleTypecheck MakeActions{..} exEnv externs m@(Module _ _ moduleName _ _) moduleIndex causedByModule = do
  progress $ CompilingModule moduleName moduleIndex causedByModule
  progress $ CompileMeta ("### CS.goBuildEnv13[" <> runModuleName moduleName <> "]")
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim m
  lint withPrim

  progress $ CompileMeta ("### CS.goDesugar1[" <> runModuleName moduleName <> "]")
  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    -- lift $ progress $ CompilingModule moduleName moduleIndex "2"
    (desugared, (exEnv', usedImports)) <- runStateT (desugar externs withPrim) (exEnv, mempty)
    lift $ progress $ CompileMeta ("### CS.goTypeCheck2[" <> runModuleName moduleName <> "]")
    -- lift $ progress $ CompilingModule moduleName moduleIndex "3"
    let modulesExports = (\(_, _, exports) -> exports) <$> exEnv'
    -- lift $ progress $ CompilingModule moduleName moduleIndex "4"
    (checked, CheckState{..}) <- runStateT (typeCheckModule modulesExports desugared) $ emptyCheckState env
    lift $ progress $ CompileMeta ("### CS.goLintImports3[" <> runModuleName moduleName <> "]")
    -- lift $ progress $ CompilingModule moduleName moduleIndex "5"
    let usedImports' = foldl' (flip $ \(fromModuleName, newtypeCtorName) ->
          M.alter (Just . (fmap DctorName newtypeCtorName :) . fold) fromModuleName) usedImports checkConstructorImportsForCoercible
    -- Imports cannot be linted before type checking because we need to
    -- known which newtype constructors are used to solve Coercible
    -- constraints in order to not report them as unused.
    censor (addHint (ErrorInModule moduleName)) $ lintImports checked exEnv' usedImports'
    lift $ progress $ CompileMeta ("### CS.goDesugarCaseGuards4[" <> runModuleName moduleName <> "]")
    return (checked, checkEnv)

  -- progress $ CompilingModule moduleName moduleIndex "6"

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated
  progress $ CompileMeta ("### CS.goCreateBindingGroups5[" <> runModuleName moduleName <> "]")

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded

  progress $ CompileMeta ("### CS.goFfiCodegen6[" <> runModuleName moduleName <> "]")
  let upstreamDBs = M.fromList $ (\e -> (efModuleName e, efOurCacheShapes e)) <$> externs
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      (optimized, nextVar'') = runSupply nextVar' $ CF.optimizeCoreFn corefn
      (renamedIdents, renamed) = renameInModule optimized
      exts = moduleToExternsFile upstreamDBs mod' env' renamedIdents
  ffiCodegen renamed

  pure ModuleCheckResult
    { mcrUpstreamEnv = env
    , mcrCheckedEnv = env'
    , mcrOriginalModule = m
    , mcrExternsInput = externs
    , mcrExEnv = exEnv
    , mcrRenamed = renamed
    , mcrExterns = exts
    , mcrNextVar = nextVar''
    }

-- | Phase B of rebuilding a single module: the backend-specific `codegen`
-- call (Erlang AST generation, optimization, pretty-printing, file I/O).
-- Takes the `ExternsFile` as an input and does not further modify it -- see
-- the NOTE at its call site about grabbing a copy of the old externs file
-- before running this if you want to diff them.
rebuildModuleCodegen
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> ModuleName
  -> ModuleCheckResult
  -> m ()
rebuildModuleCodegen MakeActions{..} moduleName ModuleCheckResult{..} = do
  progress $ CompileMeta ("### CS.goCodegen7[" <> runModuleName moduleName <> "]")
  evalSupplyT mcrNextVar $ codegen mcrUpstreamEnv mcrRenamed mcrExterns

-- | Rebuild a single module, running both phase A (typecheck) and phase B
-- (codegen) in sequence. Used by callers that don't need (or can't use) the
-- phase split that `Make.make`'s concurrent build orchestration relies on --
-- e.g. the REPL and psc-ide's fast-rebuild workflows, which rebuild one
-- module at a time outside of any `BuildPlan`.
rebuildModuleWithIndex
  :: MakeActions Make
  -> Env
  -> [ExternsFile]
  -> Module
  -> Maybe (Int, Int)
  -> RecompileReason
  -> Make ExternsFile
rebuildModuleWithIndex ma exEnv externs m@(Module _ _ moduleName _ _) moduleIndex causedByModule = do
  checkResult <- rebuildModuleTypecheck ma exEnv externs m moduleIndex causedByModule
  rebuildModuleCodegen ma moduleName checkResult
  pure (mcrExterns checkResult)

-- | Compiles in "make" mode, compiling each module separately to a @.js@ file and an @externs.cbor@ file.
--
-- If timestamps or hashes have not changed, existing externs files can be used to provide upstream modules' types without
-- having to typecheck those modules again.
make :: MakeActions Make
     -> [CST.PartialResult Module]
     -> Make [ExternsFile]
make ma@MakeActions{..} ms = do
  progress $ CompileMeta "### CS.goReadCacheDb8"

  checkModuleNames
  cacheDb <- readCacheDb
  progress $ CompileMeta "### CS.goSortModules9"

  -- let !_ = unsafePerformIO $ putStrLn (show ("cacheDb", cacheDb))

  (sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) ms
  progress $ CompileMeta "### CS.goConstructBuildPlan10"

  -- (buildPlan2, newCacheDb2) <- BuildPlan.construct2 ma cacheDb (sortedDirect, graphDirect)
  (buildPlan, newCacheDb) <- BuildPlan.construct2 ma cacheDb (sorted, graph)
  progress $ CompileMeta "### CS.goFork11"

  -- Limit concurrent module builds to the number of capabilities as
  -- (by default) inferred from `+RTS -N -RTS` or set explicitly like `-N4`.
  -- This is to ensure that modules complete fully before moving on, to avoid
  -- holding excess memory during compilation from modules that were paused
  -- by the Haskell runtime.
  capabilities <- getNumCapabilities
  let concurrency = max 1 capabilities
  lock <- C.newQSem concurrency

  let toBeRebuilt = filter (BuildPlan.needsRebuild buildPlan . getModuleName . CST.resPartial) sorted
  progress $ CompileMeta "### CS.toBeRebuilt"
  let totalModuleCount = length toBeRebuilt
  newCacheDbMVar <- newMVar cacheDb

  for_ toBeRebuilt $ \m -> fork $ (do
    -- for each module:
    -- do I need to rebuild myself?
    -- did any of my deps change?
    -- did my source files change?
    -- after recompile, did my externs change?



    let moduleName = getModuleName . CST.resPartial $ m
    -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.1"))
    -- for each module:
    -- do I need to rebuild myself?
    -- did my source files change?
    inputInfo <- getInputTimestampsAndHashes moduleName
    areMyOwnFilesUpToDate <-
      case inputInfo of
        Left RebuildNever -> do
          -- built-in module, nothing to do
          -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.2.1", "Left RebuildNever"))
          pure True
        Left RebuildAlways -> do
          -- file not yet built
          -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.2.2", "Left RebuildAlways"))
          pure False
        Right cacheInfo -> do
          -- -- progress $ CompileMeta (T.pack $ show (moduleName, -- DR.2.3", "Right cacheInfo"))
          -- file has been built before
          cwd <- liftBase getCurrentDirectory
          (newCacheInfo, isUpToDate) <- Cache.checkChanged cacheDb moduleName cwd cacheInfo
          modifyMVar_ newCacheDbMVar (\db -> pure $ M.insert moduleName newCacheInfo db)
          -- -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.4.2.1", ("writeNewCacheDb", newCacheInfo)))
          pure isUpToDate

    let bumpCompilationCounter = do
          idx <- C.takeMVar (bpIndex buildPlan)
          C.putMVar (bpIndex buildPlan) (idx + 1)


    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
    let goBuild oldExterns results recompileReason = do
          -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.4.3"))
          buildModule lock buildPlan moduleName totalModuleCount oldExterns results recompileReason
            (T.unpack . spanName . getModuleSourceSpan . CST.resPartial $ m)
            (fst $ CST.resFull m)
            (fmap importPrim . snd $ CST.resFull m)
            (deps `inOrderOf` map (getModuleName . CST.resPartial) sorted)
          -- NOTE: exception safety for this whole per-module job (including
          -- goBuild/buildModule) is handled by the `onException` wrapped around
          -- the entire forked action below, not here.

    case areMyOwnFilesUpToDate of
      False -> do
        -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.3.1", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate)))
        oldExterns <- BuildPlan.getExternFromLastSuccessfulPreviousBuild ma buildPlan moduleName
        mResults <- BuildPlan.fetchMissingExterns BuildPlan.Early ("mod", "b", moduleName) ma buildPlan deps
        case assertAllExternsExists mResults of
          Left bjRes -> bumpCompilationCounter >> BuildPlan.markCompleteImmediate ma buildPlan moduleName Nothing bjRes
          Right results -> goBuild oldExterns results SourceChangedOrDependencyFailedToBuildInPreviousCompilationOrSomethingElse
      True -> do
        -- -- progress $ CompileMeta (T.pack $ show (moduleName, -- DR.3.2", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate)))
        -- did any of my deps change?
        anyDepChanged <- BuildPlan.anyDepChanged moduleName graph buildPlan
        case anyDepChanged of
          BuildPlan.FailRebuildDepsFailed causedByModule -> do
            -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.3.2.1", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate), "FailRebuildDepsFailed", causedByModule))
            bumpCompilationCounter
            BuildPlan.markCompleteImmediate ma buildPlan moduleName Nothing BuildJobSkipped
            pure ()

          BuildPlan.FullDepsCacheHit -> do
            -- -- progress $ CompileMeta (T.pack $ show (moduleName, -- DR.3.2.2", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate), "FullDepsCacheHit"))
            bumpCompilationCounter
            BuildPlan.markCompleteImmediate ma buildPlan moduleName Nothing BuildJobSkippedFullCacheHit
            pure ()

          BuildPlan.DepsChangedPleaseRebuildIfNeeded causedByModule -> do
            -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.3.2.3", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate), "DepsChangedPleaseRebuildIfNeeded", causedByModule))
            oldExterns <- BuildPlan.getExternFromLastSuccessfulPreviousBuild ma buildPlan moduleName
            mResults <- BuildPlan.fetchMissingExterns BuildPlan.Early ("mod", "a", moduleName) ma buildPlan deps
            case assertAllExternsExists mResults of
              Left bjRes -> bumpCompilationCounter >> BuildPlan.markCompleteImmediate ma buildPlan moduleName Nothing bjRes
              Right results ->
                case BuildPlan.needsRebuildEvenAfterDiffingCacheShapes oldExterns (fmap snd results) of
                  BuildPlan.NoRebuildNeeded -> do
                    -- progress $ CompileMeta ("-- DR 3.2.3.1 upstreamDiffWasEmpty[" <> runModuleName moduleName <> "]")
                    bumpCompilationCounter
                    BuildPlan.markCompleteImmediate ma buildPlan moduleName Nothing BuildJobSkippedFullCacheHit
                    pure ()
                  BuildPlan.PleaseRebuild errs -> do
                    -- progress $ CompileMeta ("-- DR 3.2.3.2 upstreamDiffFound[" <> runModuleName moduleName <> "] " <> T.pack (show errs))
                    goBuild oldExterns results (DependencyChanged causedByModule)
    )
      -- Prevent hanging on other modules when there is an internal error
      -- anywhere in this module's build logic -- not just inside buildModule,
      -- but also e.g. in BuildPlan.fetchMissingExterns/anyDepChanged, which run
      -- before buildModule is ever called. An uncaught exception here would
      -- otherwise kill this forked thread silently, leaving this module's
      -- bjExterns/bjResult/bpCacheResult/bpExterns MVars empty forever and
      -- deadlocking every other thread that's waiting on them (e.g. in
      -- collectResults).
      `onException` BuildPlan.markCompleteImmediate ma buildPlan (getModuleName . CST.resPartial $ m) Nothing (BuildJobFailed mempty)

  -- progress $ CompileMeta (T.pack $ show ("-- DR.5", "all solo modules done, pre collection"))
  externs <- traverse tryReadMVar $ M.elems $ BuildPlan.bpExterns buildPlan
  -- progress $ CompileMeta (T.pack $ show ("-- DR.5", "all solo modules done, counts", length (filter ((==) Nothing) externs), "/", length externs, "unchanged"))
  -- Wait for all threads to complete, and collect results (and errors).

  collectedResults <- BuildPlan.collectResults buildPlan
  let directFailures =
        let
          isDirectFailure = \case
            BuildJobFailed _ -> True
            BuildJobSucceeded _ _ -> False
            BuildJobSkipped -> False
            BuildJobSkippedFullCacheHit -> False
        in
        M.filter isDirectFailure collectedResults

  -- BuildJobSkippedFullCacheHit doesn't carry its own externs (the module
  -- wasn't rebuilt, so buildModule never produced a value for it); resolve it
  -- to the on-disk externs file here (still valid, since it's a cache hit),
  -- via the same cached-or-disk-read lookup used to fetch dependencies'
  -- externs while building.
  resolvedResults <- M.traverseWithKey
    (\mn result -> case result of
        BuildJobSkippedFullCacheHit -> BuildPlan.fetchMissingExtern BuildPlan.Final ("mod", "collectResults", mn) ma buildPlan mn
        other -> pure other
    )
    collectedResults

  let (failures, successes) =
        let
          splitResults = \case
            BuildJobSucceeded _ exts ->
              Right exts
            BuildJobFailed errs ->
              Left errs
            BuildJobSkipped ->
              Left mempty
            BuildJobSkippedFullCacheHit ->
              internalError "make: BuildJobSkippedFullCacheHit should have been resolved by fetchMissingExtern above"
        in
          M.mapEither splitResults resolvedResults

  progress $ CompileMeta "### CS.collectedResults31"
  -- Write the updated build cache database to disk
  -- NOTE[drathier]: Leaving the old cache-file as-is on failed compiles is a workaround. Previously, a build error in a module caused the cache entries for all subsequent modules to be dropped, which lead to a recompile. This way, we pretend we never did that failing compile, and we'll recompile modules over and over again until we get a full successful compile. This might play badly with ide and possibly other things too, but it superficially works. It's worth a try.

  newCacheDb <- takeMVar newCacheDbMVar
  writeCacheDb $ Cache.removeModules (M.keysSet directFailures) newCacheDb
  progress $ CompileMeta "### CS.wroteCacheDB32"
  -- case () of
  --   _ | M.null failures == False ->
  --     -- NOTE[drathier]: Leaving the old cache-file as-is on failed compiles is a workaround. Previously, a build error in a module caused the cache entries for all subsequent modules to be dropped, which lead to a recompile. This way, we pretend we never did that failing compile, and we'll recompile modules over and over again until we get a full successful compile. This might play badly with ide and possibly other things too, but it superficially works. It's worth a try.
  --     pure ()
  --   _ ->
  --     writeCacheDb $ Cache.removeModules (M.keysSet failures) newCacheDb

-- caching verkar okej iom removeModules på new successful builds, men vi skriver alldeles för många filer till disk nu. Undvik att toucha och skriva över filer om innehållet ej ändrats, istället för att toucha filer för att få gamla prebuilt-logiken att funka. Ingenting räknas som prebuilt med den här logiken nu. 2023-12-24


  -- All threads have completed, rethrow any caught errors.
  let errors = M.elems failures
  unless (null errors) $ throwError (mconcat errors)

  -- Here we return all the ExternsFile in the ordering of the topological sort,
  -- so they can be folded into an Environment. This result is used in the tests
  -- and in PSCI.
  let lookupResult mn =
        fromMaybe (internalError "make: module not found in results")
        $ M.lookup mn successes
  return (map (lookupResult . getModuleName . CST.resPartial) sorted)

  where
  checkModuleNames :: Make ()
  checkModuleNames = checkNoPrim *> checkModuleNamesAreUnique

  checkNoPrim :: Make ()
  checkNoPrim =
    for_ ms $ \m ->
      let mn = getModuleName $ CST.resPartial m
      in when (isBuiltinModuleName mn) $
           throwError
             . errorMessage' (getModuleSourceSpan $ CST.resPartial m)
             $ CannotDefinePrimModules mn

  checkModuleNamesAreUnique :: Make ()
  checkModuleNamesAreUnique =
    for_ (findDuplicates (getModuleName . CST.resPartial) ms) $ \mss ->
      throwError . flip foldMap mss $ \ms' ->
        let mn = getModuleName . CST.resPartial . NEL.head $ ms'
        in errorMessage'' (fmap (getModuleSourceSpan . CST.resPartial) ms') $ DuplicateModule mn

  -- Find all groups of duplicate values in a list based on a projection.
  findDuplicates :: Ord b => (a -> b) -> [a] -> Maybe [NEL.NonEmpty a]
  findDuplicates f xs =
    case filter ((> 1) . length) . NEL.groupBy ((==) `on` f) . sortOn f $ xs of
      [] -> Nothing
      xss -> Just xss

  -- Sort a list so its elements appear in the same order as in another list.
  inOrderOf :: (Ord a) => [a] -> [a] -> [a]
  inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

  buildModule :: QSem -> BuildPlan -> ModuleName -> Int -> Maybe ExternsFile -> M.Map ModuleName (MultipleErrors, ExternsFile) -> RecompileReason -> FilePath -> [CST.ParserWarning] -> Either (NEL.NonEmpty CST.ParserError) Module -> [ModuleName] -> Make ()
  buildModule lock buildPlan moduleName cnt oldExts results recompileReason fp pwarnings mres deps = do
    progress $ CompileMeta ("### CS.goParse12[" <> runModuleName moduleName <> "]")

    -- Phase A: parse, then typecheck through ffiCodegen -- i.e. everything
    -- needed to know this module's ExternsFile. Published via
    -- markExternsComplete as soon as it's ready (below), so dependent
    -- modules waiting on our externs can proceed with their own phase A
    -- without waiting on our codegen (phase B).
    --
    -- NOTE[drathier]: catchError here only ever fires if there's an error in a module we're building; it does not fire if a module is skipped because upstream modules failed to build.
    phaseAResult <- flip catchError (return . Left) $ do
      let pwarnings' = CST.toMultipleWarnings fp pwarnings
      tell pwarnings'
      m <- CST.unwrapParserError fp mres
      -- We need to wait for dependencies to be built, before checking if the current
      -- module should be rebuilt, so the first thing to do is to wait on the
      -- MVars for the module's dependencies.

      -- We need to ensure that all dependencies have been included in Env
      C.modifyMVar_ (bpEnv buildPlan) $ \env -> do
        let
          go :: Env -> ModuleName -> Make Env
          go e dep = case M.lookup dep results of
            Just (_, exts)
              | not (M.member dep e) -> externsEnv e exts
            _ -> return e
        foldM go env deps
      env <- C.readMVar (bpEnv buildPlan)
      idx <- C.takeMVar (bpIndex buildPlan)
      C.putMVar (bpIndex buildPlan) (idx + 1)

      -- Bracket phase A's work behind the semaphore, including forcing the
      -- result, same as before this was split into two phases -- this just
      -- limits concurrency and keeps memory usage down while *this* phase
      -- runs; the permit is released again below as soon as phase A is done,
      -- so it can be picked up by another waiting module instead of being
      -- held for the whole of our codegen too.
      (checkResult, warningsA) <- bracket_ (C.waitQSem lock) (C.signalQSem lock) $ do
          -- Eventlog markers for profiling; see debug/eventlog.js
          liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " typecheck start"
          (checkResult, warningsA) <- listen $
            rebuildModuleTypecheck ma env (snd <$> M.elems results) m (Just (idx, cnt)) recompileReason
          -- Force the externs and warnings to avoid retaining excess module
          -- data after phase A is finished. (We don't force the rest of
          -- checkResult here -- the CoreFn module/docs/env it also carries
          -- for phase B -- since it's about to be consumed by phase B in
          -- this same thread anyway, so there's no cross-thread retention to
          -- guard against the way there was for the externs themselves.)
          _ <- evaluate . force $ (mcrExterns checkResult, warningsA)
          liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " typecheck end"
          return (checkResult, warningsA)

      pure $ Right (pwarnings' <> warningsA, checkResult)

    case phaseAResult of
      Left errs ->
        BuildPlan.markCompleteImmediate ma buildPlan moduleName oldExts (BuildJobFailed errs)
      Right (warningsA, checkResult) -> do
        let exts = mcrExterns checkResult
        BuildPlan.markExternsComplete ma buildPlan moduleName oldExts (BuildJobSucceeded warningsA exts)

        -- Phase B: codegen. Only runs after our externs are already
        -- published above, so it no longer blocks any dependent module --
        -- only the overall build result and our own output files depend on
        -- it now.
        phaseBResult <- flip catchError (return . Left) $ do
          ((), warningsB) <- bracket_ (C.waitQSem lock) (C.signalQSem lock) $ do
              liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " codegen start"
              r <- evaluate . force <=< listen $
                rebuildModuleCodegen ma moduleName checkResult
              liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " codegen end"
              return r
          pure $ Right warningsB

        let finalResult = case phaseBResult of
              Left errs -> BuildJobFailed errs
              Right warningsB -> BuildJobSucceeded (warningsA <> warningsB) exts

        BuildPlan.markComplete ma buildPlan moduleName finalResult

-- | Infer the foreign module file for a module by looking for a matching
-- Erlang foreign file next to the source file.
inferForeignModules
  :: forall m
   . MonadIO m
  => M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules =
    fmap (M.mapMaybe id) . traverse inferForeignModule
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> m (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = Erl.Build.inferForeignModule' path


assertAllExternsExists :: M.Map ModuleName BuildJobResult -> Either BuildJobResult (M.Map ModuleName (MultipleErrors, ExternsFile))
assertAllExternsExists externs = assertAllExternsExistsImpl (M.toList externs) mempty

assertAllExternsExistsImpl externs acc =
  case externs of
    [] -> Right acc
    ((moduleName,e):ex) ->
      case e of
        BuildJobSkippedFullCacheHit -> internalError "Make:assertAllExternsExists saw unexpected BuildJobSkippedFullCacheHit, should have been replaced with BuildJobSucceeded by now"
        BuildJobFailed err -> Left BuildJobSkipped
        BuildJobSkipped -> Left BuildJobSkipped
        --
        BuildJobSucceeded err ext -> assertAllExternsExistsImpl ex (M.insert moduleName (err, ext) acc)
