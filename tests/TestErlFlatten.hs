{-# LANGUAGE OverloadedStrings #-}

module TestErlFlatten (spec) where

import Prelude

import Data.Text (Text)
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Optimizer.Inliner (flattenFailureCases)
import Language.PureScript.PSString (mkString)

import Test.Hspec (Spec, describe, it, shouldBe)

-- A Veither is `#{type := ~"_", value := V}` when ok and `#{type := Label, value := E}` for an error.
veitherPat :: Text -> Erl -> Erl
veitherPat label v = EMapPattern [(Atom Nothing "type", EStringLiteral (mkString label)), (Atom Nothing "value", v)]

veitherVal :: Text -> Erl -> Erl
veitherVal label v = EMapLiteral [(Atom Nothing "type", EStringLiteral (mkString label)), (Atom Nothing "value", v)]

-- `maybe #{type := ~"_", value := A} ?= step(), #{type => ~"_", value => A} end`,
-- a do-block with one step that can fail.
failingBlock :: Erl
failingBlock =
  EMaybe
    [(Just (veitherPat "_" (EVar "A")), EApp RegularApp (EVar "step") [])]
    (veitherVal "_" (EVar "A"))

-- `case Scrut of Pat -> Rest; Other -> Other end`
passFailure :: Erl -> Erl -> Erl -> Erl
passFailure scrut pat rest = ECaseOf scrut [(EBinder pat, rest), (EBinder (EVar "Other"), EVar "Other")]

spec :: Spec
spec = describe "flattenFailureCases" $ do
  -- The block's early failure is its result and goes through the outer case,
  -- where `on` matches it. Splicing the block's steps into the outer `maybe`
  -- returned that failure past the `on` pattern.
  it "keeps a matched maybe block nested when the pattern matches errors" $
    flattenFailureCases
      (passFailure failingBlock (veitherPat "boom" (EVar "E")) (EApp RegularApp (EVar "handle") [EVar "E"]))
      `shouldBe` EMaybe
        [(Just (veitherPat "boom" (EVar "E")), failingBlock)]
        (EApp RegularApp (EVar "handle") [EVar "E"])

  -- Only a success pattern can't match what the block bails out with, so there
  -- its steps are the outer ones.
  it "flattens a matched maybe block when the pattern is the ok case" $
    flattenFailureCases
      (passFailure failingBlock (veitherPat "_" (EVar "V")) (EVar "V"))
      `shouldBe` EMaybe
        [ (Just (veitherPat "_" (EVar "A")), EApp RegularApp (EVar "step") []),
          (Nothing, EBind (EVar "V") (EVar "A"))
        ]
        (EVar "V")
