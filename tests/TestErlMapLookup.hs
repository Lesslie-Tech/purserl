{-# LANGUAGE OverloadedStrings #-}

module TestErlMapLookup (spec) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

sources :: [(FilePath, Text)]
sources =
  [ ( "Map.purs",
      T.unlines
        [ "module Map where",
          "data Maybe a = Nothing | Just a",
          "foreign import data Map :: Type -> Type -> Type",
          -- The rules replace calls of these, whatever their bodies.
          "lookup :: forall k v. k -> Map k v -> Maybe v",
          "lookup _ _ = Nothing",
          "lookupOr :: forall k v. v -> k -> Map k v -> v",
          "lookupOr d _ _ = d",
          "next :: Int -> Int",
          "next x = x"
        ]
    ),
    ( "User.purs",
      T.unlines
        [ "module User where",
          "import Map (Map, Maybe, lookup, lookupOr, next)",
          "byVariable :: Int -> Map Int String -> Maybe String",
          "byVariable k m = lookup k m",
          "byLiteral :: Map String Int -> Maybe Int",
          "byLiteral m = lookup \"a\" m",
          "byCall :: Int -> Map Int String -> Maybe String",
          "byCall k m = lookup (next k) m",
          "withDefault :: Int -> Map Int String -> String",
          "withDefault k m = lookupOr \"none\" k m"
        ]
    )
  ]

spec :: Spec
spec = describe "Map lookups" $ do
  it "match on the map instead of calling maps:find" $ do
    user <- compileUser
    user `shouldSatisfy` (not . T.isInfixOf "maps:find")
    -- A variable key, a literal key, and a missing key that matches any map.
    user `shouldSatisfy` T.isInfixOf "#{A_k := Val@"
    user `shouldSatisfy` T.isInfixOf "#{~\"a\" := Val@"
    T.count "#{} ->" user `shouldBe` 4

  it "binds a key that is not a variable or a literal first, so that it is evaluated before the map" $ do
    user <- compileUser
    user `shouldSatisfy` T.isInfixOf "Key@"
    user `shouldSatisfy` T.isInfixOf "map@ps:next("

compileUser :: IO Text
compileUser = do
  let dir = ".test_modules" </> "erl-map-lookup"
      srcDir = dir </> "src"
      outDir = dir </> "output"
  removePathForcibly dir
  createDirectoryIfMissing True srcDir
  paths <- traverse (\(f, c) -> TIO.writeFile (srcDir </> f) c >> pure (srcDir </> f)) sources
  moduleFiles <- traverse (\p -> (,) p <$> TIO.readFile p) paths
  (result, _) <- P.runMake P.defaultOptions $ do
    ms <- liftEither $ CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList [(P.getModuleName $ CST.resPartial pm, Right fp) | (fp, pm) <- ms]
    foreigns <- liftIO $ P.inferForeignModules filePathMap
    P.make (P.buildMakeActions outDir filePathMap foreigns True Nothing) (map snd ms)
  case result of
    Left errs -> expectationFailure (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> pure ""
    Right _ -> TIO.readFile (outDir </> "User" </> "user@ps.erl")
