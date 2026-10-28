module Language.PureScript.Erl.CodeGen.Optimizer.Common where

import Control.Arrow (second)
import Control.Monad.State.Strict (State, put, runState)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Common (atomPS)
import Language.PureScript.PSString (PSString)
import Prelude.Compat

{-# INLINE isFn #-}
isFn :: (Text, Text) -> Erl -> Bool
isFn (moduleName, fnName) (EApp _ (EAtomLiteral (Atom (Just x) y)) []) =
  x == moduleName && y == fnName
isFn _ _ = False

{-# INLINE isDict #-}
isDict :: (Text, PSString) -> Erl -> Bool
isDict (moduleName, dictName) (EApp _ (EAtomLiteral (Atom (Just x) y)) []) = x == moduleName && y == atomPS dictName
isDict _ _ = False

{-# INLINE isUncurriedFn #-}
isUncurriedFn :: (Text, PSString) -> Erl -> Bool
isUncurriedFn = isFnName

{-# INLINE isFnName #-}
isFnName :: (Text, PSString) -> Erl -> Bool
isFnName (moduleName, dictName) (EAtomLiteral (Atom (Just x) y)) = x == moduleName && y == atomPS dictName
isFnName _ _ = False

{-# INLINE isUncurriedFn' #-}
isUncurriedFn' :: (Text, Text) -> Erl -> Bool
isUncurriedFn' (moduleName, fnName) (EAtomLiteral (Atom (Just x) y)) = x == moduleName && y == fnName
isUncurriedFn' _ _ = False

{-# INLINE applyAll #-}
applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)


-- Check if var x occurs in expression
occurs :: Text -> Erl -> Bool
occurs x =
  snd . flip runState False . everywhereOnErlTopDownM go
  where
    go :: Erl -> State Bool Erl
    go e@(EVar x') | x == x' = do
      put True
      pure e
    go e = pure e


-- TODO figure this into generic traversal with context pattern
replaceIdents :: [(Text, Erl)] -> Erl -> Erl
replaceIdents unfilteredVars = go
  where
    vars = filter (\(k,v) -> k /= "_") unfilteredVars
    -- Placeholder
    f :: Erl -> Erl
    f = id

    go :: Erl -> Erl
    go (EFunFull fname args) = f $ EFunFull fname $ map goFunHead args
    go (ECaseOf e binds) = f $ ECaseOf (go e) $ map goCase binds
    go (EUnary op e) = f $ EUnary op (go e)
    go (EBinary op e1 e2) = f $ EBinary op (go e1) (go e2)
    go (EFunctionDef t ssann a ss e) = f $ EFunctionDef t ssann a ss (go e)
    go (EVarBind x e) = f $ EVarBind x (go e)
    go (EBind x e) = f $ EBind (go x) (go e)
    go (EApp meta e es) = f $ EApp meta (go e) (map go es)
    go (EBlock es) = f $ EBlock (map go es)
    go (ETupleLiteral es) = f $ ETupleLiteral (map go es)
    go (EMapLiteral binds) = f $ EMapLiteral $ map (second go) binds
    go (EMapPattern binds) = f $ EMapPattern $ map (second go) binds
    go (EMapUpdate e binds) = f $ EMapUpdate (go e) $ map (second go) binds
    go (EListLiteral es) = f $ EListLiteral (map go es)
    go (EArrayLiteral es) = f $ EArrayLiteral (map go es)
    go (EListCons es e) = f $ EListCons (map go es) (go e)
    go (ETryAnyAny e1 e2) = f $ ETryAnyAny (go e1) (go e2)
    go (EAndThen e1 e2) = f $ EAndThen (go e1) (go e2)
    go (ELet e1 e2) = f $ ELet (go e1) (go e2)
    go v@(EVar var) = fromMaybe v $ lookup var vars
    go (ERawErlangSource fmt binds) = f $ ERawErlangSource fmt $ map (second go) binds
    go (EMaybe kind steps final) = f $ EMaybe kind (map (\(p, e) -> (fmap go p, go e)) steps) (go final)
    go other = other

    -- -- Vars are *not* fresh inf case binders
    goCase :: (EBinder, Erl) -> (EBinder, Erl)
    goCase (EBinder e, e') = (EBinder (go e), go e')
    -- goCase (EGuardedBinder e (Guard eg), e') = (EGuardedBinder (go e) (Guard $ go eg), go e')

    goFunHead :: (EFunBinder, Erl) -> (EFunBinder, Erl)
    goFunHead (EFunBinder es, e) = (EFunBinder es, replaceIdents vars' e)
      where
        vars' = filter (\(var, _) -> not $ any (occurs var) es) vars
{-
    goFunHead (EFunBinder es g, e) = (EFunBinder es g', replaceIdents vars' e)
      where
        vars' = filter (\(var, _) -> not $ any (occurs var) es) vars
        g' = (\(Guard eg) -> Just $ Guard $ replaceIdents vars' eg) =<< g
-}


collect :: Int -> Erl -> Erl
collect n e = go e []
  where
    go :: Erl -> [Erl] -> Erl
    go (EApp meta f x) acc | not (null x) =
      case x <> acc of
        args | length args >= n -> EApp meta f args
        args -> go f args
    go _other _acc = e
