module TestEmptyClassDictElision (spec) where

import Prelude

import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.CoreFn (Ann, Bind(..), Expr(..), Module(..))
import Language.PureScript.Names (Ident, runIdent)

import System.Directory (createDirectoryIfMissing)

import Test.Hspec (Spec, context, expectationFailure, specify)

scratchDir :: FilePath
scratchDir = ".test_modules/empty-class-dict-elision"

-- | Type-check a single, self-contained source module (no external
-- dependencies beyond Prim) and return its post-typecheck, renamed CoreFn --
-- i.e. exactly the tree shape 'Language.PureScript.CoreFn.Desugar' and the
-- entailment/generalization code in 'TypeChecker.{Entailment,Types}' hand
-- off to codegen, without going through backend-specific (Erlang) codegen.
--
-- A constraint like `(Partial => a) -> a` can only be discharged through an
-- FFI escape hatch (see `Partial.Unsafe`'s real-world implementation), so a
-- test exercising that shape needs a `foreign import` and a (never actually
-- compiled/run) `.erl` file backing it -- pass its content as `mFfiContent`.
compileToCoreFn :: Maybe T.Text -> T.Text -> IO (Either P.MultipleErrors (Module Ann))
compileToCoreFn mFfiContent src =
  case sequence $ CST.parseFromFile "Test.purs" src of
    Left parseError ->
      pure $ Left (CST.toMultipleErrors "Test.purs" parseError)
    Right (_warnings, m) -> do
      let moduleName = P.getModuleName m
          filePathMap = M.singleton moduleName (Left P.RebuildAlways)
      foreigns <- case mFfiContent of
        Nothing -> pure M.empty
        Just ffiContent -> do
          createDirectoryIfMissing True scratchDir
          let ffiPath = scratchDir <> "/Foreign.erl"
          TIO.writeFile ffiPath ffiContent
          pure $ M.singleton moduleName ffiPath
      let makeActions = P.buildMakeActions scratchDir filePathMap foreigns False Nothing
      (result, _) <- P.runMake P.defaultOptions $
        P.rebuildModuleTypecheck makeActions P.primEnv [] m Nothing P.UnknownRecompileReason
      pure $ P.mcrRenamed <$> result

-- | Find a top-level binding by name, recursive or not.
findBind :: T.Text -> [Bind Ann] -> Maybe (Expr Ann)
findBind name = foldr go Nothing
  where
  go (NonRec _ ident expr) found
    | identMatches ident = Just expr
    | otherwise = found
  go (Rec binds) found =
    case [ expr | ((_, ident), expr) <- binds, identMatches ident ] of
      (expr : _) -> Just expr
      [] -> found
  identMatches :: Ident -> Bool
  identMatches ident = runIdent ident == name

compileAndFindBind :: Maybe T.Text -> T.Text -> T.Text -> (Expr Ann -> IO ()) -> IO ()
compileAndFindBind mFfiContent src name assertion = do
  result <- compileToCoreFn mFfiContent src
  case result of
    Left errs ->
      expectationFailure $ "Compilation failed:\n" <> P.prettyPrintMultipleErrors P.defaultPPEOptions errs
    Right m ->
      case findBind name (moduleDecls m) of
        Nothing -> expectationFailure $ "No binding named " <> T.unpack name <> " found in CoreFn output"
        Just expr -> assertion expr

spec :: Spec
spec = context "Empty class dictionary elision" $ do
  -- Regression test for the Entailment.hs/Types.hs 'typesOf' generalization
  -- path: an *inferred* (unannotated) binding that uses an empty-class
  -- constrained function. Before the fix, the elided dictionary was still
  -- bound as a real parameter here, so `inferredUser` compiled to a 2-ary
  -- function (dict, x) instead of a 1-ary one (x).
  specify "an inferred binding does not bind a parameter for an empty-class dictionary" $ do
    let src = T.unlines
          [ "module Test.EmptyClassDictElision1 where"
          , "class MyEmptyClass"
          , "useEmpty :: MyEmptyClass => Int -> Int"
          , "useEmpty x = x"
          , "inferredUser x = useEmpty x"
          ]
    compileAndFindBind Nothing src "inferredUser" $ \case
      Abs _ _ (App _ _ _) -> pure ()
      other -> expectationFailure $ "Expected a 1-argument function (Abs _ _ (App _ _ _)), got: " <> show other

  -- Regression test matching the exact shape that broke `Partial.Unsafe`'s
  -- `_unsafePartial` FFI in pay-backend: an *anonymous* value (here, a
  -- lambda) whose constraint is nested inside its type only because it's
  -- being checked against another function's constrained argument type
  -- (`discharge :: forall a. (c => a) -> a`), not because the lambda itself
  -- has a top-level signature. This is `check''`s `ConstrainedType` case.
  -- Before the fix, this lambda was wrapped in an extra Abs for the dict,
  -- silently changing its arity from 1 to 2 -- which is exactly what broke
  -- `_unsafePartial`'s `F(unit)` discharge pattern at runtime.
  specify "an anonymous value passed where an empty-class-constrained argument is expected keeps its own arity" $ do
    let src = T.unlines
          [ "module Test.EmptyClassDictElision2 where"
          , "class MyEmptyClass"
          , "foreign import dischargeImpl :: forall a b. a -> b"
          , "discharge :: forall a. (MyEmptyClass => a) -> a"
          , "discharge = dischargeImpl"
          , "useIt :: Int -> Int"
          , "useIt = discharge (\\y -> y)"
          ]
        ffiContent = "'dischargeImpl'(X) -> X.\n"
    compileAndFindBind (Just ffiContent) src "useIt" $ \case
      App _ _ (Abs _ _ (Var _ _)) -> pure ()
      other -> expectationFailure $ "Expected the lambda argument to stay a 1-argument function (App _ _ (Abs _ _ (Var _ _))), got: " <> show other
