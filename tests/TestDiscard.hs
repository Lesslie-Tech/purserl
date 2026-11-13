module TestDiscard (spec) where

import Prelude

import Control.Monad (void)

import Data.Map qualified as M
import Data.Text qualified as T

import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST

import Test.Hspec (Spec, expectationFailure, shouldSatisfy, specify)

-- | Type-check a single module (no dependencies beyond Prim). The module is
-- named `Data.Unit` so that it defines the `Unit` type the do-block check
-- refers to.
typecheck :: T.Text -> IO (Either P.MultipleErrors ())
typecheck src =
  case sequence $ CST.parseFromFile "Test.purs" src of
    Left parseError ->
      pure $ Left (CST.toMultipleErrors "Test.purs" parseError)
    Right (_warnings, m) -> do
      let filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)
          makeActions = P.buildMakeActions ".test_modules/discard" filePathMap M.empty False Nothing
      (result, _) <- P.runMake P.defaultOptions $
        P.rebuildModuleTypecheck makeActions P.primEnv [] m Nothing P.UnknownRecompileReason
      pure $ void result

spec :: Spec
spec =
  specify "do statements are desugared to bind and must be Unit" $ do
    let prelude =
          [ "module Data.Unit where"
          , "data Unit = Unit"
          , "data Box a = Box a"
          , "bind :: forall a b. Box a -> (a -> Box b) -> Box b"
          , "bind (Box a) f = f a"
          , "tick :: Box Unit"
          , "tick = Box Unit"
          ]
    -- No `discard` is in scope, so this only compiles if statements use `bind`.
    typecheck (T.unlines $ prelude <> ["ok :: Box Int", "ok = do", "  tick", "  Box 1"]) >>= \case
      Left errs -> expectationFailure $ P.prettyPrintMultipleErrors P.defaultPPEOptions errs
      Right () -> pure ()
    typecheck (T.unlines $ prelude <> ["bad :: Box Int", "bad = do", "  Box 1", "  Box 2"]) >>= \case
      Left errs -> do
        let rendered = T.pack (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
        rendered `shouldSatisfy` T.isInfixOf "implicitly discarded"
        -- Reported at the offending statement (line 10), not the start of the block.
        rendered `shouldSatisfy` T.isInfixOf "Test.purs:10:3"
      Right () -> expectationFailure "Expected a non-Unit discarded statement to be rejected"
    -- An explicit `_ <-` is for non-Unit results only.
    typecheck (T.unlines $ prelude <> ["ok2 :: Box Int", "ok2 = do", "  _ <- Box 1", "  Box 2"]) >>= \case
      Left errs -> expectationFailure $ P.prettyPrintMultipleErrors P.defaultPPEOptions errs
      Right () -> pure ()
    typecheck (T.unlines $ prelude <> ["bad2 :: Box Int", "bad2 = do", "  _ <- tick", "  Box 2"]) >>= \case
      Left errs -> do
        let rendered = T.pack (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
        rendered `shouldSatisfy` T.isInfixOf "is redundant"
        rendered `shouldSatisfy` T.isInfixOf "Test.purs:10:3"
      Right () -> expectationFailure "Expected `_ <-` on a Unit result to be rejected"
    -- The result type may only be known after later unification.
    typecheck (T.unlines $ prelude <> ["bad3 :: Box Int", "bad3 = do", "  _ <- Box (id Unit)", "  Box 2", "id :: forall a. a -> a", "id a = a"]) >>= \case
      Left errs -> do
        let rendered = T.pack (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
        rendered `shouldSatisfy` T.isInfixOf "is redundant"
      Right () -> expectationFailure "Expected `_ <-` on an inferred Unit result to be rejected"
