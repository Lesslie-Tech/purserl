{-# LANGUAGE TemplateHaskell #-}

-- |
-- Data types for roles.
--
module Language.PureScript.Roles
  ( Role(..)
  , displayRole
  ) where

import Prelude

import Control.DeepSeq (NFData)
import Data.Aeson qualified as A
import Data.Aeson.TH qualified as A
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.PureScript.Interning (Intern(..))

-- |
-- The role of a type constructor's parameter.
data Role
  = Nominal
  -- ^ This parameter's identity affects the representation of the type it is
  -- parameterising.
  | Representational
  -- ^ This parameter's representation affects the representation of the type it
  -- is parameterising.
  | Phantom
  -- ^ This parameter has no effect on the representation of the type it is
  -- parameterising.
  deriving (Show, Eq, Ord, Generic)

instance NFData Role
instance Intern Role

instance Binary Role where
  put = \case
    Nominal -> putWord8 0
    Representational -> putWord8 1
    Phantom -> putWord8 2
  get = getWord8 >>= \case
    0 -> pure Nominal
    1 -> pure Representational
    2 -> pure Phantom
    n -> fail ("Binary Role: invalid tag " <> show n)

$(A.deriveJSON A.defaultOptions ''Role)

displayRole :: Role -> Text
displayRole r = case r of
  Nominal -> "nominal"
  Representational -> "representational"
  Phantom -> "phantom"
