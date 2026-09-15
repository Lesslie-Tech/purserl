-- | The data type of compiler options
module Language.PureScript.Options where

import Prelude
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as Map

-- | The data type of compiler options
data Options = Options
  { optionsVerboseErrors :: Bool
  -- ^ Verbose error message
  , optionsNoComments :: Bool
  -- ^ Remove the comments from the generated js
  , optionsCodegenTargets :: S.Set CodegenTarget
  -- ^ Codegen targets (Erl, etc.)
  } deriving Show

-- Default make options
defaultOptions :: Options
defaultOptions = Options False False (S.singleton Erl)

data CodegenTarget = Erl
  deriving (Eq, Ord, Show)

codegenTargets :: Map String CodegenTarget
codegenTargets = Map.fromList
  [ ("erl", Erl)
  ]
