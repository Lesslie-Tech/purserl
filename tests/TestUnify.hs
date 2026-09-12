module TestUnify (spec) where

import Prelude

import Data.IntMap.Strict qualified as IM
import Language.PureScript.TypeChecker.Monad (Substitution(..), emptySubstitution)
import Language.PureScript.TypeChecker.Unify (substituteType)
import Language.PureScript.Types (srcTUnknown, srcTypeApp, srcTypeVar)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "substituteType" $ do
  it "fully resolves unknowns nested inside another unknown's solution" $ do
    -- Regression test: unification variable 1 is solved to a compound type
    -- (`TypeApp (TUnknown 2) leafB`) that itself embeds unification variable
    -- 2, which is *also* already solved (to `leafA`). Reading variable 1's
    -- solution must chase variable 2's solution too, not just return the
    -- compound type with an unresolved `TUnknown 2` still inside it -- that
    -- under-resolution previously slipped past `unifyTypes`, which visits
    -- the tree structurally and resolves unknowns as it goes anyway, but
    -- broke every other caller of `substituteType` (e.g. instance/row
    -- entailment in `TypeChecker.Entailment`), which expects a fully
    -- resolved type back and doesn't itself re-descend into the result.
    let leafA = srcTypeVar "leafA"
        leafB = srcTypeVar "leafB"
        sub = emptySubstitution
          { substType = IM.fromList
              [ (1, srcTypeApp (srcTUnknown 2) leafB)
              , (2, leafA)
              ]
          }
    substituteType sub (srcTUnknown 1) `shouldBe` srcTypeApp leafA leafB
