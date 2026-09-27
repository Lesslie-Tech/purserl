-- |
-- Concrete monad for the desugaring pipeline. Kept separate from 'Sugar.hs'
-- itself to avoid an import cycle, since modules like 'Sugar.AdoNotation'
-- import this and are in turn imported by 'Sugar.hs'.
--
module Language.PureScript.Sugar.Monad (DesugarM) where

import Control.Monad.State.Strict (StateT)
import Control.Monad.Supply (SupplyT)
import Language.PureScript.Linter.UsedImports (UsedImports)
import Language.PureScript.Make.Monad (Make)
import Language.PureScript.Sugar.Names.Env (Env)

type DesugarM = StateT (Env, UsedImports) (SupplyT Make)
