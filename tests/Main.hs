{-# LANGUAGE DoAndIfThenElse #-}

module Main (main) where

import Prelude

import Test.Hspec

import TestAst qualified
import TestCoreFn qualified
import TestCst qualified
import TestHierarchy qualified
import TestIde qualified
import TestMake qualified
import TestUnify qualified
import TestGraph qualified

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  hspec $ do
    describe "cst" TestCst.spec
    describe "ast" TestAst.spec
    describe "ide" TestIde.spec
    describe "make" TestMake.spec
    describe "unify" TestUnify.spec
    describe "corefn" TestCoreFn.spec
    describe "hierarchy" TestHierarchy.spec
    describe "graph" TestGraph.spec
