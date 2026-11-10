-- | Dropping @A = B,@ where both are variables, and @A = begin ..., B end,@, which the printer turns into one.
module Language.PureScript.Erl.CodeGen.Optimizer.Aliases (dropVarAliases) where

import Data.Bifunctor (bimap, first)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Optimizer.Common (replaceIdents)
import Prelude.Compat

-- | A variable bound to another one, or to a block that ends in one, is that variable, so its uses are replaced. Variables are bound once, and
-- only a fun can bind a name again (in its head, or as its own name): when either name is bound by one of
-- those inside what follows, the alias stays.
dropVarAliases :: Erl -> Erl
dropVarAliases = everywhereOnErl $ \case
  ELet (EBind (EVar a) (EVar b)) rest | aliasable a b rest -> replaceIdents [(a, EVar b)] rest
  ELet (EBind (EVar a) rhs) rest
    | Just (pre, b) <- endsInVar rhs, aliasable a b rest -> foldr ELet (replaceIdents [(a, EVar b)] rest) pre
  EBlock es -> EBlock (inBlock es)
  EMaybe kind steps final -> case inMaybe steps final of
    ([], final') -> final'
    (steps', final') -> EMaybe kind steps' final'
  e -> e
  where
    -- A step without a pattern is a statement of the block.
    inMaybe steps final = case steps of
      (Nothing, EBind (EVar a) (EVar b)) : rest
        | aliasable a b (EMaybe ExitsOnFailure rest final) ->
            let sub = replaceIdents [(a, EVar b)] in inMaybe (map (bimap (fmap sub) sub) rest) (sub final)
      (Nothing, EBind (EVar a) rhs) : rest
        | Just (pre, b) <- endsInVar rhs,
          aliasable a b (EMaybe ExitsOnFailure rest final) ->
            let sub = replaceIdents [(a, EVar b)] in inMaybe (map (Nothing,) pre <> map (bimap (fmap sub) sub) rest) (sub final)
      step : rest -> let (rest', final') = inMaybe rest final in (step : rest', final')
      [] -> ([], final)
    inBlock = \case
      EBind (EVar a) (EVar b) : rest@(_ : _)
        | aliasable a b (EBlock rest) -> inBlock (map (replaceIdents [(a, EVar b)]) rest)
      EBind (EVar a) rhs : rest@(_ : _)
        | Just (pre, b) <- endsInVar rhs, aliasable a b (EBlock rest) -> inBlock (pre <> map (replaceIdents [(a, EVar b)]) rest)
      e : rest -> e : inBlock rest
      [] -> []

-- | The statements of a block that ends in a variable, and the variable.
endsInVar :: Erl -> Maybe ([Erl], Text)
endsInVar e = do
  (pre, lst) <- go e
  case lst of
    EVar b | not (null pre) -> Just (pre, b)
    _ -> Nothing
  where
    go = \case
      EBlock es@(_ : _) -> Just (init es, last es)
      ELet s r -> Just (maybe ([s], r) (first (s :)) (go r))
      _ -> Nothing

aliasable :: Text -> Text -> Erl -> Bool
aliasable a b rest = a /= b && a /= "_" && b /= "_" && not (any (`elem` funBound) [a, b])
  where
    funBound = bindersOfFuns rest

-- | The names that funs in the expression bind.
bindersOfFuns :: Erl -> [Text]
bindersOfFuns = everything (<>) $ \case
  EFunFull name clauses -> maybeToList name <> concat [concatMap varsIn patterns | (EFunBinder patterns, _) <- clauses]
  _ -> []
  where
    varsIn = everything (<>) $ \case
      EVar v -> [v]
      _ -> []
