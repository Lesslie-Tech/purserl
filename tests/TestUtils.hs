module TestUtils where

import Prelude

import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function (on)
import Data.List (sortBy, stripPrefix, groupBy)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, takeDirectory, takeExtensions, (</>))
import System.IO.Error (isDoesNotExistError)
import System.FilePath.Glob qualified as Glob
import Test.Hspec (Expectation, HasCallStack, expectationFailure, pendingWith)

getTestFiles :: FilePath -> IO [[FilePath]]
getTestFiles testDir = do
  let dir = "tests" </> "purs" </> testDir
  getFiles dir <$> testGlob dir
  where
  -- A glob for all purs and js files within a test directory
  testGlob :: FilePath -> IO [FilePath]
  testGlob = Glob.globDir1 (Glob.compile "**/*.purs")
  -- Groups the test files so that a top-level file can have dependencies in a
  -- subdirectory of the same name. The inner tuple contains a list of the
  -- .purs files and the .js files for the test case.
  getFiles :: FilePath -> [FilePath] -> [[FilePath]]
  getFiles baseDir
    = map (filter ((== ".purs") . takeExtensions) . map (baseDir </>))
    . groupBy ((==) `on` extractPrefix)
    . sortBy (compare `on` extractPrefix)
    . map (makeRelative baseDir)
  -- Extracts the filename part of a .purs file, or if the file is in a
  -- subdirectory, the first part of that directory path.
  extractPrefix :: FilePath -> FilePath
  extractPrefix fp =
    let dir = takeDirectory fp
        ext = reverse ".purs"
    in if dir == "."
       then maybe fp reverse $ stripPrefix ext $ reverse fp
       else dir

-- | Assert that the contents of the provided file path match the result of the
-- provided action. If the "HSPEC_ACCEPT" environment variable is set, or if the
-- file does not already exist, we write the resulting ByteString out to the
-- provided file path instead. However, if the "CI" environment variable is
-- set, "HSPEC_ACCEPT" is ignored and we require that the file does exist with
-- the correct contents (see #3808). Based (very loosely) on the tasty-golden
-- package.
goldenVsString
  :: HasCallStack -- For expectationFailure; use the call site for better failure locations
  => FilePath
  -> IO ByteString
  -> Expectation
goldenVsString goldenFile testAction = do
  accept <- isJust <$> lookupEnv "HSPEC_ACCEPT"
  ci <- isJust <$> lookupEnv "CI"
  goldenContents <- tryJust (guard . isDoesNotExistError) (BS.readFile goldenFile)
  case goldenContents of
    Left () ->
      -- The golden file does not exist
      if ci
        then expectationFailure $ "Missing golden file: " ++ goldenFile
        else createOrReplaceGoldenFile

    Right _ | not ci && accept ->
      createOrReplaceGoldenFile

    Right expected -> do
      actual <- testAction
      if expected == actual
        then pure ()
        else expectationFailure $
          "Test output differed from '" ++ goldenFile ++ "'; got:\n" ++
          T.unpack (T.decodeUtf8With (\_ _ -> Just '\xFFFD') actual)
  where
  createOrReplaceGoldenFile = do
    testAction >>= BS.writeFile goldenFile
    pendingWith "Accepting new output"
