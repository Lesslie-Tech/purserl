-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.Erl.CodeGen.Optimizer (optimize) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.Erl.CodeGen.AST
    ( everywhereOnErl, Erl(..), pattern EApp, Atom )
import Language.PureScript.Erl.CodeGen.Optimizer.Inliner
    ( inlineCommonOperators,
      inlineCommonValuesTopDown,
      inlineCommonValuesBottomUp,
      specialize,
      inlineCommonFnsM )

import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Data.Map (Map)
import Control.Monad ((<=<))
import Debug.Trace
import Data.Function ((&))

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: MonadSupply m => [(Atom, Int)] -> [Erl] -> m [Erl]
-- optimize exports es = pure es
-- optimize exports es = pure (Inliner.inline es)
optimize exports es = do 
  es
    & mapM
      (\b ->
        b
          & inlineCommonOperators EC.effect EC.effectDictionaries id
          & specialize
          & untilFix go
          & inlineCommonFnsM id
      )

  where
  go erl =
    erl
      & inlineCommonValuesTopDown id -- expander
      & inlineCommonValuesBottomUp id -- expander
      -- Compilation took 107841 ms -- only topdown
      -- Compilation took 104441 ms -- only topdown
      -- Compilation took 102926 ms -- only bottomup
      -- Compilation took 102995 ms -- only bottomup
      -- Compilation took 109079 ms -- both


untilFixedPoint :: Show a => (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go 10
  where
  go 0 a = pure a -- trace (show ("untilFixedPoint bailed out", a)) $ pure a
  go n a = do
   a' <- f a
   if a' == a then return a' else go (n-1) a'

untilFix :: Show a => Eq a => (a -> a) -> a -> a
untilFix f = go 10
  where
  go 0 a = a -- trace (show ("untilFix bailed out", a)) $ a
  go n a =
   let a2 = f a in
   if a2 == a then a2 else go (n-1) a2


-- |
-- Take all top-level ASTs and return a function for expanding top-level
-- variables during the various inlining steps in `optimize`.
--
-- Everything that gets inlined as an optimization is of a form that would
-- have been lifted to a top-level binding during CSE, so for purposes of
-- inlining we can save some time by only expanding variables bound at that
-- level and not worrying about any inner scopes.
--
buildExpander :: [Erl] -> Erl -> Erl
buildExpander = replaceAtoms . foldr go []
  where
  go = \case
    EFunctionDef _ _ name [] e | isSimpleApp e  -> ((name, e) :)
    _ -> id

  replaceAtoms updates = everywhereOnErl (replaceAtom updates)

  replaceAtom updates = \case
    EApp _ (EAtomLiteral a) [] | Just e <- lookup a updates
      -> replaceAtoms updates e
    other -> other

  -- simple nested applications that look similar to floated synthetic apps
  isSimpleApp (EApp _ e1 es) = isSimpleApp e1 && all isSimpleApp es
  isSimpleApp (EAtomLiteral _) = True
  isSimpleApp _ = False
