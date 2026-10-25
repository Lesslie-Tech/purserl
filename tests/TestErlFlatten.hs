{-# LANGUAGE OverloadedStrings #-}

module TestErlFlatten (spec) where

import Prelude

import Data.Text (Text)
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.PSString (mkString)

import Language.PureScript.Erl.CodeGen.Optimizer.Inliner (flattenFailureCases, mergeHandlerCases)
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
    ExitsOnFailure
    [(Just (veitherPat "_" (EVar "A")), EApp RegularApp (EVar "step") [])]
    (veitherVal "_" (EVar "A"))

-- `case Scrut of Pat -> Rest; Other -> Other end`
passFailure :: Erl -> Erl -> Erl -> Erl
passFailure scrut pat rest = ECaseOf scrut [(EBinder pat, rest), (EBinder (EVar "Other"), EVar "Other")]

spec :: Spec
spec = do
  describe "flattenFailureCases" $ do
    -- The block's early failure is its result and goes through the outer case,
    -- where `on` matches it. As a `maybe` step the `on` would match the block's
    -- final value only, and the failure would be returned past it.
    it "leaves a case on an error pattern alone" $ do
      let onBoom = passFailure failingBlock (veitherPat "boom" (EVar "E")) (EApp RegularApp (EVar "handle") [EVar "E"])
      flattenFailureCases onBoom `shouldBe` onBoom

    -- Only a success pattern can't match what the block bails out with, so there
    -- its steps are the outer ones.
    it "flattens a case on the ok pattern, and the maybe block it matches" $
      flattenFailureCases
        (passFailure failingBlock (veitherPat "_" (EVar "V")) (EVar "V"))
        `shouldBe` EMaybe
          ExitsOnFailure
          [ (Just (veitherPat "_" (EVar "A")), EApp RegularApp (EVar "step") []),
            (Nothing, EBind (EVar "V") (EVar "A"))
          ]
          (EVar "V")

    -- With steps after it, an `on` as a `?=` step would return the ok value of the
    -- block it handles as the result of everything, and skip those steps.
    it "keeps an `on` case as the value of the ok step it feeds" $ do
      let onBoom = passFailure failingBlock (veitherPat "boom" (EVar "E")) (veitherVal "_" (EVar "E"))
      flattenFailureCases (passFailure onBoom (veitherPat "_" (EVar "V")) (EApp RegularApp (EVar "next") [EVar "V"]))
        `shouldBe` EMaybe
          ExitsOnFailure
          [(Just (veitherPat "_" (EVar "V")), onBoom)]
          (EApp RegularApp (EVar "next") [EVar "V"])

    -- An alt chain returns the first value that is not a failure, so what it
    -- returns still has to be matched; its steps can't be the outer ones.
    it "keeps an alt chain matched by an ok step as its value" $ do
      let nothing = ETupleLiteral [EAtomLiteral (Atom Nothing "nothing")]
          just v = ETupleLiteral [EAtomLiteral (Atom Nothing "just"), v]
          altChain = EMaybe ExitsOnSuccess [(Just nothing, EVar "A")] (EVar "B")
      flattenFailureCases (passFailure altChain (just (EVar "X")) (EApp RegularApp (EVar "next") [EVar "X"]))
        `shouldBe` EMaybe ExitsOnFailure [(Just (just (EVar "X")), altChain)] (EApp RegularApp (EVar "next") [EVar "X"])

    -- The last step of a chain of binds can be an alt chain. Its early exit is a
    -- success, so it stays the final expression and isn't merged into the chain's steps.
    it "keeps an alt chain that ends a chain of binds nested" $ do
      let nothing = ETupleLiteral [EAtomLiteral (Atom Nothing "nothing")]
          altChain = EMaybe ExitsOnSuccess [(Just nothing, EVar "A")] (EVar "B")
      flattenFailureCases (passFailure failingBlock (veitherPat "_" (EVar "V")) altChain)
        `shouldBe` EMaybe
          ExitsOnFailure
          [ (Just (veitherPat "_" (EVar "A")), EApp RegularApp (EVar "step") []),
            (Nothing, EBind (EVar "V") (EVar "A"))
          ]
          altChain

  describe "mergeHandlerCases" $ do
    let handler label v rhs = (EBinder (veitherPat label (EVar v)), rhs)
        pass x = (EBinder (EVar x), EVar x)
        handled v = veitherVal "_" (EVar v)
    -- A handled value is ok, so a later handler's arm can't match it.
    it "merges chained on handlers into one case with an arm each" $
      mergeHandlerCases
        ( ECaseOf
            (ECaseOf (EVar "E") [handler "a" "A" (handled "A"), pass "X"])
            [handler "b" "B" (handled "B"), pass "Y"]
        )
        `shouldBe` ECaseOf (EVar "E") [handler "a" "A" (handled "A"), handler "b" "B" (handled "B"), pass "Y"]

    -- What an `onThen` handler returns can be an error that the next handler is for.
    it "keeps an onThen handler's arm, whose result is not known to be ok, nested" $ do
      let nested =
            ECaseOf
              (ECaseOf (EVar "E") [handler "a" "A" (EApp RegularApp (EVar "again") [EVar "A"]), pass "X"])
              [handler "b" "B" (handled "B"), pass "Y"]
      mergeHandlerCases nested `shouldBe` nested
