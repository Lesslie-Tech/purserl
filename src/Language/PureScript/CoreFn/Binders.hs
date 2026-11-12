{-# LANGUAGE DeriveAnyClass #-}
-- |
-- The core functional representation for binders
--
module Language.PureScript.CoreFn.Binders where

import Prelude

import Language.PureScript.AST.Literals (Literal)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Names (Ident, ProperName, ProperNameType(..), Qualified)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- |
-- Data type for binders
--
data Binder a
  -- |
  -- Wildcard binder
  --
  = NullBinder a
  -- |
  -- A binder which matches a literal value
  --
  | LiteralBinder a (Literal (Binder a))
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder a Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder a (Qualified (ProperName 'TypeName)) (Qualified (ProperName 'ConstructorName)) [Binder a]
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder a Ident (Binder a)
  -- |
  -- A native list cons pattern
  --
  | ConsBinder a (Binder a) (Binder a)
  -- |
  -- A string prefix pattern
  --
  | StringPrefixBinder a PSString (Binder a)
  -- |
  -- A map entry pattern: a literal key and its value
  --
  | MapEntryBinder a (Binder a) (Binder a)
  -- |
  -- Two patterns that both have to match
  --
  | AndBinder a (Binder a) (Binder a)
  deriving (Eq, Ord, Show, Functor, Generic, NFData)
