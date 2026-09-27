-- |
-- Kept separate from 'Language.PureScript.Linter.Imports' (which re-exports
-- it) so that "Language.PureScript.Sugar.Monad" can depend on this type
-- without an import cycle -- 'Language.PureScript.Linter.Imports' itself
-- imports "Language.PureScript.Sugar.Names.Imports".
--
module Language.PureScript.Linter.UsedImports (UsedImports) where

import Data.Map (Map)
import Language.PureScript.Names (ModuleName, Name, Qualified)

-- |
-- Map of module name to list of imported names from that module which have
-- been used.
--
type UsedImports = Map ModuleName [Qualified Name]
