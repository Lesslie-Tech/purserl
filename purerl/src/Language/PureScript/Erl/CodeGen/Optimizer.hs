-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.Erl.CodeGen.Optimizer (optimize, OwnModule (..)) where

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
import Data.Text (Text)
import Language.PureScript.PSString (PSString)

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
-- The rules match a call of a function of the module being compiled as `self@ps:f(..)`, but the code
-- generator calls its own functions without a module and its own foreign imports through `@foreign`.
-- The rules run on `canonicalizeCalls`, and `localizeCalls` goes back to the code generator's spelling.
optimize :: OwnModule -> [(Atom, Int)] -> [Erl] -> Supply [Erl]
-- optimize exports es = pure es
-- optimize exports es = pure (Inliner.inline es)
optimize own exports es = do
  let symbolOf = dictSymbol (Map.fromList [(canonicalAtom own a, s) | (a, s) <- mapMaybe topLevelDict es])
  es
    & mapM
      (\b ->
        fmap (localizeCalls own . go) (b
          & canonicalizeCalls own
          & inlineCommonOperators EC.effect EC.effectDictionaries id
          & specialize
          & inlineCommonFnsM symbolOf)
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


-- | The module being compiled.
data OwnModule = OwnModule
  { -- | Its Erlang module of compiled PureScript, `m@ps`.
    ownPs :: Text,
    -- | Its Erlang module of foreign imports, `m@foreign`.
    ownForeign :: Text,
    -- | The arity of each of its foreign imports.
    ownForeignArities :: Map Text Int
  }

canonicalAtom :: OwnModule -> Atom -> Atom
canonicalAtom own = \case
  Atom Nothing f -> Atom (Just (ownPs own)) f
  Atom (Just m) f | m == ownForeign own -> Atom (Just (ownPs own)) f
  a -> a

-- | Every call and function reference to the module's own functions, and foreign imports, is to `m@ps`.
canonicalizeCalls :: OwnModule -> Erl -> Erl
canonicalizeCalls own = everywhereOnErl $ \case
  EApp meta (EAtomLiteral a) args -> EApp meta (EAtomLiteral (canonicalAtom own a)) args
  EFunRef a n -> EFunRef (canonicalAtom own a) n
  e -> e

-- | The inverse of `canonicalizeCalls`. The code generator calls a foreign import through `m@foreign` in a
-- saturated call, and in the zero-parameter function that forwards to the import. Calls that the rules built
-- can also be saturated calls of an import.
localizeCalls :: OwnModule -> Erl -> Erl
localizeCalls own def = everywhereOnErl lower def
  where
    lower = \case
      EApp meta (EAtomLiteral (Atom (Just m) f)) args
        | m == ownPs own -> EApp meta (EAtomLiteral (atomOf f (forwards f || (not (null args) && arityIs f (length args))))) args
      EFunRef (Atom (Just m) f) n
        | m == ownPs own -> EFunRef (atomOf f (forwards f)) n
      e -> e

    atomOf f direct = if direct then Atom (Just (ownForeign own)) f else Atom Nothing f
    arityIs f n = Map.lookup f (ownForeignArities own) == Just n

    -- The function that forwards to the foreign import `f` is not a function of the module's PureScript, so
    -- whatever it calls or refers to by that name is the import.
    forwards f = case def of
      EFunctionDef _ _ (Atom _ name) [] _ -> name == f && Map.member f (ownForeignArities own)
      _ -> False

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
