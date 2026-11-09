{-# LANGUAGE OverloadedStrings #-}

module TestErlHeadMatches (spec) where

import Prelude

import Data.Text (Text)
import Language.PureScript.Erl.CodeGen (fuseHead)
import Language.PureScript.Erl.CodeGen.AST
import Test.Hspec (Spec, describe, it, shouldBe)

-- | `#{k := X}`
record :: Erl
record = EMapPattern [(Atom Nothing "k", EVar "X")]

tuple :: [Erl] -> Erl
tuple = ETupleLiteral

call :: Text -> Erl -> Erl
call f x = EApp RegularApp (EAtomLiteral (Atom Nothing f)) [x]

-- | `case scrutinee of pat -> rhs`
arm :: Erl -> Erl -> Erl -> Erl
arm scrutinee pat rhs = ECaseOf scrutinee [(EBinder pat, rhs)]

spec :: Spec
spec = describe "parameters that the body matches" $ do
  it "puts the pattern in the head" $
    fuseHead ["V"] (arm (EVar "V") record (call "h" (EVar "X"))) `shouldBe` ([record], call "h" (EVar "X"))

  it "keeps the name of a parameter that is used again" $
    fuseHead ["V"] (arm (EVar "V") record (call "h" (EVar "V"))) `shouldBe` ([EBind (EVar "V") record], call "h" (EVar "V"))

  it "fuses the columns of a case on several parameters" $
    fuseHead ["V", "W"] (arm (tuple [EVar "V", EVar "W"]) (tuple [record, tuple [EVar "Y"]]) (call "h" (EVar "X")))
      `shouldBe` ([record, tuple [EVar "Y"]], call "h" (EVar "X"))

  it "leaves the parameter of a column that matches nothing" $
    fuseHead ["V", "W"] (arm (tuple [EVar "V", EVar "W"]) (tuple [EVar "_", record]) (call "h" (EVar "X")))
      `shouldBe` ([EVar "V", record], call "h" (EVar "X"))

  it "leaves a case on something that isn't a parameter" $
    let body = arm (EVar "W") record (call "h" (EVar "X"))
     in fuseHead ["V"] body `shouldBe` ([EVar "V"], body)

  it "leaves a pattern that mentions the parameter" $
    let body = arm (EVar "V") (tuple [EVar "V"]) (call "h" (EVar "V"))
     in fuseHead ["V"] body `shouldBe` ([EVar "V"], body)

  -- The optimizer drops the match of a pattern with no variable, as for any other case with one arm.
  it "leaves a pattern with no variable" $
    let body = arm (EVar "V") (EAtomLiteral (Atom Nothing "false")) (call "h" (EVar "V"))
     in fuseHead ["V"] body `shouldBe` ([EVar "V"], body)

  it "leaves a case with more than one arm" $
    let body = ECaseOf (EVar "V") [(EBinder record, call "h" (EVar "X")), (EBinder (EVar "_"), call "g" (EVar "V"))]
     in fuseHead ["V"] body `shouldBe` ([EVar "V"], body)

  it "leaves a body that is not a case" $
    fuseHead ["V", "W"] (call "h" (EVar "V")) `shouldBe` ([EVar "V", EVar "W"], call "h" (EVar "V"))
