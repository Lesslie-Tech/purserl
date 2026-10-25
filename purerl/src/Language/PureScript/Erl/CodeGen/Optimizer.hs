-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.Erl.CodeGen.Optimizer (optimize) where

import Prelude.Compat

import Control.Monad.Supply (Supply)

import Language.PureScript.Erl.CodeGen.AST
    ( everywhereOnErl, Erl(..), pattern EApp, Atom(..), EFunBinder(..) )
import Language.PureScript.Erl.CodeGen.Optimizer.Inliner
    ( inlineCommonOperators,
      inlineCommonValuesTopDown,
      inlineCommonValuesBottomUp,
      specialize,
      inlineCommonFnsM,
      flattenFailureCases,
      mergeHandlerCases )

import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Data.Map (Map)
import Control.Monad ((<=<))
import Debug.Trace
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Language.PureScript.PSString (PSString)

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: [(Atom, Int)] -> [Erl] -> Supply [Erl]
-- optimize exports es = pure es
-- optimize exports es = pure (Inliner.inline es)
optimize exports es = do 
  let symbolOf = dictSymbol (Map.fromList (mapMaybe topLevelDict es))
  es
    & mapM
      (\b ->
        b
          & inlineCommonOperators EC.effect EC.effectDictionaries id
          & specialize
          & inlineCommonFnsM symbolOf
          & fmap go
      )

  where
  -- A single pass is enough: every rewrite re-applies the rules to what it
  -- builds, so a second round never changes anything. Keep it that way
  -- (re-run `convert` on new nodes) instead of looping to a fixed point.
  -- Order: inlineCommonFnsM (libRules, which build lambdas) before this, so
  -- `applySpine` beta-reduces them; a rule that creates a redex below its own
  -- result must reduce it itself, as nothing revisits already-visited children.
  go erl =
    erl
      & inlineCommonValuesTopDown id -- expander
      & inlineCommonValuesBottomUp id -- expander
      & flattenFailureCases
      & mergeHandlerCases
      -- Compilation took 107841 ms -- only topdown
      -- Compilation took 104441 ms -- only topdown
      -- Compilation took 102926 ms -- only bottomup
      -- Compilation took 102995 ms -- only bottomup
      -- Compilation took 109079 ms -- both


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

-- |
-- The label of an `IsSymbol` dictionary: either a literal
-- `#{reflectSymbol => fun (_) -> ~"label" end}` or a call to a top-level
-- constant of the current module that is one.
--
dictSymbol :: Map Atom PSString -> Erl -> Maybe PSString
dictSymbol known = \case
  EApp _ (EAtomLiteral a) [] -> Map.lookup a known
  e -> literalSymbol e

literalSymbol :: Erl -> Maybe PSString
literalSymbol = \case
  EMapLiteral [(_, EFunFull Nothing [(EFunBinder [_], EStringLiteral s)])] -> Just s
  _ -> Nothing

topLevelDict :: Erl -> Maybe (Atom, PSString)
topLevelDict = \case
  EFunctionDef _ _ name [] e | Just s <- literalSymbol e -> Just (name, s)
  _ -> Nothing
