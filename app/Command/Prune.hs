module Command.Prune (command) where

import Prelude

import Control.Applicative (some)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.PureScript.Make.Actions (cacheDbFile)
import Language.PureScript.Make.Cache (removeModules)
import Language.PureScript.Make.Monad (readBinaryFileIO, writeBinaryFileIO)
import Language.PureScript.Names (ModuleName, moduleNameFromString, runModuleName)
import Options.Applicative qualified as Opts
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data PruneOptions = PruneOptions
  { pruneOutputDir :: FilePath
  , pruneModules   :: [ModuleName]
  }

prune :: PruneOptions -> IO ()
prune PruneOptions{..} = do
  let dbFile = cacheDbFile pruneOutputDir
  mdb <- readBinaryFileIO dbFile
  case mdb of
    Nothing -> do
      hPutStrLn stderr ("purs prune: no cache database found at " <> dbFile)
      exitFailure
    Just db -> do
      let requested = S.fromList pruneModules
          removed = requested `S.intersection` M.keysSet db
          missing = requested `S.difference` removed
      writeBinaryFileIO dbFile (removeModules requested db)
      mapM_ (\mn -> putStrLn ("Removed " <> T.unpack (runModuleName mn) <> " from the cache database"))
        (S.toList removed)
      mapM_ (\mn -> hPutStrLn stderr ("purs prune: module not in cache database, skipping: " <> T.unpack (runModuleName mn)))
        (S.toList missing)

moduleName :: Opts.Parser ModuleName
moduleName = moduleNameFromString . T.pack <$> Opts.strArgument
  ( Opts.metavar "MODULE..."
 <> Opts.help "Module name(s) to remove from the cache database"
  )

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

command :: Opts.Parser (IO ())
command = prune <$> (Opts.helper <*> pruneOptions)
  where
  pruneOptions :: Opts.Parser PruneOptions
  pruneOptions = PruneOptions <$> outputDirectory <*> some moduleName
