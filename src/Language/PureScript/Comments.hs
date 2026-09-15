{-# LANGUAGE TemplateHaskell #-}

-- |
-- Defines the types of source code comments
--
module Language.PureScript.Comments
  ( Comment(..)
  , convertComments
  ) where

import Prelude
import Codec.Serialise (Serialise)
import Control.Category ((>>>))
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Data.Aeson.TH (Options(..), SumEncoding(..), defaultOptions, deriveJSON)
import Language.PureScript.Interning (Intern(..))

data Comment
  = LineComment Text
  | BlockComment Text
  deriving (Show, Eq, Ord, Generic)

instance NFData Comment
instance Serialise Comment
instance Intern Comment

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)

-- |
-- Extract a documentation string (the @|@-prefixed lines) from a list of
-- comments, if any are present.
--
convertComments :: [Comment] -> Maybe Text
convertComments cs = do
  let raw = concatMap toLines cs
  let docs = mapMaybe stripPipe raw
  guard (not (null docs))
  pure (T.unlines docs)

  where
  toLines (LineComment s) = [s]
  toLines (BlockComment s) = T.lines s

  stripPipe =
    T.dropWhile (== ' ')
    >>> T.stripPrefix "|"
    >>> fmap (dropPrefix " ")

  dropPrefix prefix str =
    fromMaybe str (T.stripPrefix prefix str)
