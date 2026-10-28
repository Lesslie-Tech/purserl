-- |
-- Common pretty-printing utility functions
--
module Language.PureScript.Pretty.Common where

import Prelude

import Data.Text (Text)

import Language.PureScript.CST.Lexer (isUnquotedKey)

import Text.PrettyPrint.Boxes (Box(..), emptyBox, text, top, vcat, (//))
import Text.PrettyPrint.Boxes qualified as Box

parensT :: Text -> Text
parensT s = "(" <> s <> ")"

objectKeyRequiresQuoting :: Text -> Bool
objectKeyRequiresQuoting = not . isUnquotedKey

-- | Place a box before another, vertically when the first box takes up multiple lines.
before :: Box -> Box -> Box
before b1 b2 | rows b1 > 1 = b1 // b2
             | otherwise = b1 Box.<> b2

beforeWithSpace :: Box -> Box -> Box
beforeWithSpace b1 = before (b1 Box.<> text " ")

-- | Place a Box on the bottom right of another
endWith :: Box -> Box -> Box
endWith l r = l Box.<> vcat top [emptyBox (rows l - 1) (cols r), r]
