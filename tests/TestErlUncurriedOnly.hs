{-# LANGUAGE OverloadedStrings #-}

module TestErlUncurriedOnly (spec) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum, isUpper)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Environment
  ( Environment (..),
    NameKind (External),
    NameVisibility (Defined),
    function,
    initEnvironment,
    makeTypeClassData,
    tyInt,
  )
import Language.PureScript.Erl.CodeGen (buildCodegenEnvironment, isUncurriedOnly)
import Language.PureScript.Names (Ident (..), ModuleName (..), ProperName (..), Qualified (..), QualifiedBy (..))
import Language.PureScript.Types (srcConstraint, srcConstrainedType)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

modName :: ModuleName
modName = ModuleName "M"

qual :: Text -> Qualified Ident
qual = Qualified (ByModuleName modName) . Ident

-- A class with a method `method`, and a name in each shape the decision looks at.
env :: Environment
env =
  initEnvironment
    { typeClasses = M.singleton (Qualified (ByModuleName modName) (ProperName "C")) (makeTypeClassData [] [(Ident "method", intTo tyInt)] [] [] False),
      names =
        M.fromList
          [ (qual "two", declared (intTo (intTo tyInt))),
            (qual "one", declared (intTo tyInt)),
            (qual "value", declared tyInt),
            (qual "dictOnly", declared (srcConstrainedType (srcConstraint (Qualified (ByModuleName modName) (ProperName "C")) [] [] Nothing) tyInt)),
            (qual "method", declared (intTo tyInt))
          ]
    }
  where
    intTo = function tyInt
    declared t = (t, External, Defined)

-- Plain top-level functions are the only names that lose their curried forms.
spec :: Spec
spec = do
  describe "isUncurriedOnly" $ do
    let uncurried = isUncurriedOnly (buildCodegenEnvironment env) . qual

    it "applies to a function of several arguments" $ uncurried "two" `shouldBe` True
    it "applies to a function of one argument" $ uncurried "one" `shouldBe` True
    -- Its only parameter is the dictionary, and it is called with it all the same.
    it "applies to a function that only takes dictionaries" $ uncurried "dictOnly" `shouldBe` True
    it "does not apply to a value that isn't a function" $ uncurried "value" `shouldBe` False
    -- The dictionary holds the member, so it is reached through an accessor taking the dictionary alone.
    it "does not apply to a class member" $ uncurried "method" `shouldBe` False
    it "does not apply to a name the environment doesn't know" $ uncurried "missing" `shouldBe` False

  describe "generated code" $ do
    it "defines functions in their uncurried form only" $ do
      (lib, _) <- compileLibAndUser
      -- No curried `pick/0` and no forwarder, neither defined nor exported.
      lib `shouldSatisfy` T.isInfixOf "pick/2"
      lib `shouldSatisfy` (not . T.isInfixOf "pick/0")
      lib `shouldSatisfy` (not . T.isInfixOf "pick() ->")

    -- `unN k (N n)` is a case on all its parameters, whose variable patterns only name them.
    it "takes its parameters by the names the body uses, with no aliases of them" $ do
      (lib, user) <- compileLibAndUser
      aliasLines lib `shouldBe` []
      aliasLines user `shouldBe` []

    it "calls them with all their arguments, however they are used" $ do
      (_, user) <- compileLibAndUser
      -- Saturated (`full`), partially applied (`partial`, `later`) and as a value (`value`).
      argCounts "lib@ps:pick(" user `shouldBe` replicate 5 2

    it "evaluates the arguments of a partial application once, where it is made" $ do
      (_, user) <- compileLibAndUser
      T.count "lib@ps:pick(7, 8)" user `shouldBe` 1

    it "calls a class member through the dictionary alone" $ do
      (_, user) <- compileLibAndUser
      argCounts "lib@ps:size(" user `shouldBe` [1]

-- | The statements that only give a variable another name, @X = Y,@.
aliasLines :: Text -> [Text]
aliasLines code = [l | l <- T.strip <$> T.lines code, [x, "=", y] <- [T.words (T.dropWhileEnd (== ',') l)], isVar x, isVar y]
  where
    isVar v = maybe False (\(c, _) -> isUpper c) (T.uncons v) && T.all (\c -> isAlphaNum c || c `elem` ("_@" :: String)) v

-- | The number of arguments of each call of the function, given its text up to and including the opening parenthesis.
argCounts :: Text -> Text -> [Int]
argCounts call = map (count . T.unpack) . drop 1 . T.splitOn call
  where
    count = go (0 :: Int) 1
    go _ n [] = n
    go depth n (c : cs)
      | c `elem` ("({[" :: String) = go (depth + 1) n cs
      | c `elem` (")}]" :: String) = if depth == 0 then n else go (depth - 1) n cs
      | c == ',' && depth == 0 = go depth (n + 1) cs
      | otherwise = go depth n cs

sources :: [(FilePath, Text)]
sources =
  [ ( "Lib.purs",
      T.unlines
        [ "module Lib where",
          "pick :: Int -> Int -> Int",
          "pick a _ = a",
          "newtype N = N Int",
          "swap :: Int -> Int -> Int",
          "swap a b = pick b a",
          "unN :: Int -> N -> Int",
          "unN k (N n) = pick k n",
          "class Sized a where",
          "  size :: a -> Int -> Int"
        ]
    ),
    ( "User.purs",
      T.unlines
        [ "module User where",
          "import Lib (class Sized, pick, size)",
          "full :: Int",
          "full = pick 1 2",
          "partial :: Int -> Int",
          "partial = pick 1",
          "later :: Int -> { g :: Int -> Int }",
          "later _ = { g: pick (pick 7 8) }",
          "value :: { f :: Int -> Int -> Int }",
          "value = { f: pick }",
          "viaClass :: forall a. Sized a => a -> Int",
          "viaClass x = size x 1"
        ]
    )
  ]

-- | Compile the modules, returning the Erlang code of @Lib@ and of @User@.
compileLibAndUser :: IO (Text, Text)
compileLibAndUser = do
  let dir = ".test_modules" </> "erl-uncurried-only"
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
    Left errs -> expectationFailure (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> pure ("", "")
    Right _ -> (,) <$> TIO.readFile (outDir </> "Lib" </> "lib@ps.erl") <*> TIO.readFile (outDir </> "User" </> "user@ps.erl")
