{-# LANGUAGE OverloadedStrings #-}

module TestErlIsSymbol (spec) where

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
  [ ( "Symbol.purs",
      T.unlines
        [ "module Data.Symbol (class IsSymbol, reflect) where",
          "class IsSymbol (sym :: Symbol) where",
          "  reflect :: String"
        ]
    ),
    ( "User.purs",
      T.unlines
        [ "module User where",
          "import Data.Symbol (class IsSymbol, reflect)",
          "data Proxy (s :: Symbol) = Proxy",
          "known :: String",
          "known = reflect @\"abc\"",
          "polymorphic :: forall s. IsSymbol s => Proxy s -> String",
          "polymorphic _ = reflect @s",
          "viaDictionary :: String",
          "viaDictionary = polymorphic (Proxy :: Proxy \"xyz\")"
        ]
    )
  ]

spec :: Spec
spec = describe "IsSymbol" $ do
  it "reflects a known symbol to the string" $ do
    user <- compileUser
    user `shouldSatisfy` T.isInfixOf "known() ->\n  ~\"abc\""

  -- The dictionary of a symbol is the symbol itself, not a function that ignores its argument.
  it "builds the dictionary of a symbol literal from the string" $ do
    user <- compileUser
    user `shouldSatisfy` T.isInfixOf "#{reflect => ~\"xyz\"}"

compileUser :: IO Text
compileUser = do
  let dir = ".test_modules" </> "erl-is-symbol"
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
