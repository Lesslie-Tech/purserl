{-# LANGUAGE DoAndIfThenElse #-}

module Main (main) where

import Prelude

import Test.Hspec

import TestAst qualified
import TestCoreFn qualified
import TestCst qualified
import TestDocs qualified
import TestHierarchy qualified
import TestPrimDocs qualified
import TestIde qualified
import TestPscPublish qualified
-- import TestBundle qualified
import TestMake qualified
import TestUtils qualified
import TestGraph qualified

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  TestUtils.updateSupportCode

  hspec $ do
    describe "cst" TestCst.spec
    describe "ast" TestAst.spec
    describe "ide" TestIde.spec
    describe "make" TestMake.spec
    describe "corefn" TestCoreFn.spec
    describe "docs" TestDocs.spec
    describe "prim-docs" TestPrimDocs.spec
    describe "publish" TestPscPublish.spec
    describe "hierarchy" TestHierarchy.spec
    describe "graph" TestGraph.spec
