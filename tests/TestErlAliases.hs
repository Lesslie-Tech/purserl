{-# LANGUAGE OverloadedStrings #-}

module TestErlAliases (spec) where

import Prelude

import Data.Text (Text)
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Optimizer.Aliases (dropVarAliases)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | `A = B, rest`
alias :: Text -> Text -> Erl -> Erl
alias a b = ELet (EBind (EVar a) (EVar b))

use :: Text -> Erl
use x = EApp RegularApp (EVar "f") [EVar x]

spec :: Spec
spec = describe "dropVarAliases" $ do
  it "replaces the uses of the alias" $
    dropVarAliases (alias "A" "B" (use "A")) `shouldBe` use "B"

  it "follows a chain of aliases" $
    dropVarAliases (alias "A" "B" (alias "C" "A" (use "C"))) `shouldBe` use "B"

  it "drops an alias in a block, and in a maybe block's plain steps" $ do
    dropVarAliases (EBlock [EBind (EVar "A") (EVar "B"), use "A", use "A"]) `shouldBe` EBlock [use "B", use "B"]
    dropVarAliases (EMaybe ExitsOnFailure [(Nothing, EBind (EVar "A") (EVar "B")), (Nothing, use "A")] (use "A"))
      `shouldBe` EMaybe ExitsOnFailure [(Nothing, use "B")] (use "B")

  it "keeps a binding of anything but a variable" $ do
    let notAlias = ELet (EBind (EVar "A") (use "B")) (use "A")
    dropVarAliases notAlias `shouldBe` notAlias
    dropVarAliases (ELet (EBind (ETupleLiteral [EVar "A"]) (EVar "B")) (use "A"))
      `shouldBe` ELet (EBind (ETupleLiteral [EVar "A"]) (EVar "B")) (use "A")

  -- `fun (B) -> A end` would take the `B` of its own after the replacement.
  it "keeps an alias of a name that a fun inside binds again" $ do
    let captured = alias "A" "B" (EFun1 Nothing "B" (use "A"))
    dropVarAliases captured `shouldBe` captured

  it "keeps an alias to a name that a fun inside binds, and one to the name of a fun" $ do
    let shadowed = alias "A" "B" (EFun1 Nothing "A" (use "A"))
        named = alias "A" "B" (EFun1 (Just "A") "X" (use "A"))
    dropVarAliases shadowed `shouldBe` shadowed
    dropVarAliases named `shouldBe` named

  it "leaves the wildcard alone" $ do
    let wild = alias "_" "B" (use "B")
    dropVarAliases wild `shouldBe` wild

  -- The printer splices `A = begin P, B end` into `P, A = B`.
  it "drops the binding of a variable to a block that ends in a variable" $ do
    let pre = EBind (ETupleLiteral [EVar "R"]) (EVar "S")
    dropVarAliases (ELet (EBind (EVar "A") (EBlock [pre, EVar "R"])) (use "A"))
      `shouldBe` ELet pre (use "R")
    dropVarAliases (ELet (EBind (EVar "A") (EBlock [pre, use "R"])) (use "A"))
      `shouldBe` ELet (EBind (EVar "A") (EBlock [pre, use "R"])) (use "A")
