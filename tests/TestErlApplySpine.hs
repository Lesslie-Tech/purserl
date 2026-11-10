{-# LANGUAGE OverloadedStrings #-}

module TestErlApplySpine (spec) where

import Prelude

import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Optimizer.Inliner (applySpine)
import Test.Hspec (Spec, describe, it, shouldBe)

call :: Erl -> [Erl] -> Erl
call = EApp RegularApp

-- | `(case S of A -> fun (_) -> V end; B -> g end)(Arg)`
applied :: Erl -> Erl -> Erl -> Erl
applied armA armB arg = call (ECaseOf (EVar "S") [(EBinder (EVar "A"), armA), (EBinder (EVar "B"), armB)]) [arg]

spec :: Spec
spec = describe "applySpine" $ do
  let fun = EFun1 Nothing "X" (EVar "V")
      -- `X = Arg, V`, which a later rewrite simplifies.
      bound = ELet (EBind (EVar "X") (EVar "Arg")) (EVar "V")

  it "applies each arm of a case when every arm is a function" $
    applySpine (applied fun fun (EVar "Arg"))
      `shouldBe` Just (ECaseOf (EVar "S") [(EBinder (EVar "A"), bound), (EBinder (EVar "B"), bound)])

  -- The closure of the function arm goes away; the other arm is the call it was.
  it "applies each arm of a case when only some arm is a function" $
    applySpine (applied fun (EVar "g") (EVar "Arg"))
      `shouldBe` Just (ECaseOf (EVar "S") [(EBinder (EVar "A"), bound), (EBinder (EVar "B"), call (EVar "g") [EVar "Arg"])])

  it "leaves a case with no function arm alone" $
    applySpine (applied (EVar "f") (EVar "g") (EVar "Arg")) `shouldBe` Nothing

  -- The argument would be evaluated once per arm.
  it "leaves a case alone when the argument is not a variable or literal" $
    applySpine (applied fun fun (call (EVar "h") [])) `shouldBe` Nothing
