{-# LANGUAGE OverloadedStrings #-}

module TestErlPatternOperators (spec) where

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

-- | The library modules that define the list and map types. They define no
-- operators, the pattern operators are built into the compiler.
library :: [(FilePath, Text)]
library =
  [ ( "List.purs",
      T.unlines
        [ "module List where",
          "foreign import data List :: Type -> Type"
        ]
    ),
    ( "Map.purs",
      T.unlines
        [ "module Map where",
          "foreign import data Map :: Type -> Type -> Type"
        ]
    )
  ]

user :: [Text] -> (FilePath, Text)
user body =
  ( "User.purs",
    T.unlines $
      [ "module User where",
        "import List (List)",
        "import Map (Map)"
      ]
        <> body
  )

-- | Compile the library and a user module, returning the Erlang of the latter.
compileUser :: [Text] -> IO (Either Text Text)
compileUser body = do
  let dir = ".test_modules" </> "erl-pattern-operators"
      srcDir = dir </> "src"
      outDir = dir </> "output"
  removePathForcibly dir
  createDirectoryIfMissing True srcDir
  paths <- traverse (\(f, c) -> TIO.writeFile (srcDir </> f) c >> pure (srcDir </> f)) (library <> [user body])
  moduleFiles <- traverse (\p -> (,) p <$> TIO.readFile p) paths
  (result, _) <- P.runMake P.defaultOptions $ do
    ms <- liftEither $ CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList [(P.getModuleName $ CST.resPartial pm, Right fp) | (fp, pm) <- ms]
    foreigns <- liftIO $ P.inferForeignModules filePathMap
    P.make (P.buildMakeActions outDir filePathMap foreigns True Nothing) (map snd ms)
  case result of
    Left errs -> pure . Left . T.pack $ P.prettyPrintMultipleErrors P.defaultPPEOptions errs
    Right _ -> Right <$> TIO.readFile (outDir </> "User" </> "user@ps.erl")

expectErlang :: [Text] -> [Text] -> IO ()
expectErlang body expected =
  compileUser body >>= \case
    Left err -> expectationFailure (T.unpack err)
    Right erl -> mapM_ (\e -> erl `shouldSatisfy` T.isInfixOf e) expected

expectError :: [Text] -> Text -> IO ()
expectError body message =
  compileUser body >>= \case
    Left err -> err `shouldSatisfy` T.isInfixOf message
    Right _ -> expectationFailure $ "Expected an error mentioning: " <> T.unpack message

spec :: Spec
spec = describe "Operator patterns in Erlang" $ do
  it "matches list heads and tails with native list patterns" $
    expectErlang
      [ "f :: List Int -> Int",
        "f (_ : y : _) = y",
        "f _ = 0"
      ]
      ["[_, B_y | _] ->"]

  it "matches a string prefix with a binary pattern, merging nested prefixes" $
    expectErlang
      [ "f :: String -> String",
        "f (\"ab\" <> \"c\" <> rest) = rest",
        "f _ = \"\""
      ]
      ["<<\"abc\"/utf8, B_rest/binary>> ->"]

  it "matches several map entries with one map pattern" $
    expectErlang
      [ "f :: Map String Int -> Int",
        "f (\"a\" := x & \"b\" := y) = y",
        "f _ = 0"
      ]
      ["#{~\"a\" := B_x, ~\"b\" := C_y} ->"]

  it "nests map patterns, matches literal values and binds the whole map" $
    expectErlang
      [ "f :: Map String (Map Int String) -> String",
        "f (\"o\" := (1 := s & 2 := _)) = s",
        "f _ = \"\"",
        "g :: Map String Int -> Map String Int",
        "g m@(\"k\" := 1 & \"z\" := _) = m",
        "g m = m",
        "h :: Map String (List Int) -> Int",
        "h (\"l\" := x : _ & \"k\" := _) = x",
        "h _ = 0"
      ]
      [ "#{~\"o\" := #{1 := B_s, 2 := _}} ->",
        "B_m = #{~\"k\" := 1, ~\"z\" := _} ->",
        "#{~\"l\" := [B_x | _], ~\"k\" := _} ->"
      ]

  it "does not combine records with &" $
    expectError
      [ "f :: { a :: Int, b :: Int } -> Int",
        "f ({ a: x } & { b: _ }) = x",
        "f _ = 0"
      ]
      "can only combine map entries"

  it "requires a literal map key" $
    expectError ["f :: Map String Int -> Int", "f (k := _) = 1"] "must be a literal"

  it "only combines map patterns with &" $
    expectError ["f :: List Int -> Int", "f (_ : _ & _ : _) = 1", "f _ = 0"] "can only combine map entries"

  it "only lets `<>` follow a string literal" $
    expectError ["f :: String -> String", "f (x <> _) = x"] "must be a string literal followed by"

  it "types map entries against the map's key and value types" $ do
    expectError ["f :: Map String Int -> Int", "f (\"a\" := \"x\") = 1"] "Could not match type"
    expectError ["f :: String -> Int", "f (\"a\" := _) = 1"] "Could not match type"

  it "rejects the operators that alias functions or constructors" $ do
    expectError
      [ "g :: Int -> Int -> Int",
        "g a _ = a",
        "infixr 5 g as +++",
        "f :: Int -> Int",
        "f (1 +++ _) = 1",
        "f _ = 0"
      ]
      "cannot be used in a pattern"
    expectError
      [ "data Pair a b = Pair a b",
        "infixr 5 Pair as :*",
        "f :: Pair Int Int -> Int",
        "f (a :* _) = a"
      ]
      "cannot be used in a pattern"
