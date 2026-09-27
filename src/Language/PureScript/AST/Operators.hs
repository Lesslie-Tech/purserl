-- |
-- Operators fixity and associativity
--
module Language.PureScript.AST.Operators where

import Prelude

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)

import Language.PureScript.Crash (internalError)
import Language.PureScript.Interning (Intern(..))

-- |
-- A precedence level for an infix operator
--
type Precedence = Integer

-- |
-- Associativity for infix operators
--
data Associativity = Infixl | Infixr | Infix
  deriving (Show, Eq, Ord, Generic)

instance NFData Associativity
instance Intern Associativity

instance Binary Associativity where
  put = \case
    Infixl -> putWord8 0
    Infixr -> putWord8 1
    Infix -> putWord8 2
  get = getWord8 >>= \case
    0 -> pure Infixl
    1 -> pure Infixr
    2 -> pure Infix
    n -> fail ("Binary Associativity: invalid tag " <> show n)

showAssoc :: Associativity -> String
showAssoc Infixl = "infixl"
showAssoc Infixr = "infixr"
showAssoc Infix  = "infix"

readAssoc :: String -> Associativity
readAssoc "infixl" = Infixl
readAssoc "infixr" = Infixr
readAssoc "infix"  = Infix
readAssoc _ = internalError "readAssoc: no parse"

instance A.ToJSON Associativity where
  toJSON = A.toJSON . showAssoc

instance A.FromJSON Associativity where
  parseJSON = fmap readAssoc . A.parseJSON

-- |
-- Fixity data for infix operators
--
data Fixity = Fixity !Associativity !Precedence
  deriving (Show, Eq, Ord, Generic)

instance NFData Fixity

instance Binary Fixity where
  put (Fixity a b) = put a >> put b
  get = Fixity <$> get <*> get

instance A.ToJSON Fixity where
  toJSON (Fixity associativity precedence) =
    A.object [ "associativity" .= associativity
             , "precedence" .= precedence
             ]
