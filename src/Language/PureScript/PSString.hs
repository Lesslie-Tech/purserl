module Language.PureScript.PSString
  ( PSString
  , toUTF16CodeUnits
  , decodeString
  , decodeStringEither
  , decodeStringWithReplacement
  , prettyPrintString
  , prettyPrintStringJS
  , mkString
  --
  , fromString
  , fromText
  ) where

import Prelude
import GHC.Generics (Generic)
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Bits (shiftR, (.&.))
import Data.Char qualified as Char
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Numeric (showHex)
import Data.Aeson qualified as A

-- |
-- Strings in PureScript are, per the language spec, sequences of UTF-16 code
-- units (matching JavaScript's string semantics), which in principle
-- includes unpaired surrogates that don't form valid Unicode text. This
-- compiler only targets Erlang, whose string/binary literals are UTF-8 (a
-- lone surrogate can't be encoded in UTF-8 at all, so codegen already
-- replaces one with U+FFFD if it ever reaches a string literal), so there is
-- no benefit to carrying that fidelity through the whole compiler pipeline.
-- `PSString` is backed directly by `Text`; any lone surrogate in a literal
-- (an obscure, deliberately-malformed edge case) is replaced with U+FFFD at
-- construction time instead of forcing every ordinary string in the compiler
-- to pay for a boxed `[Word16]` representation.
--
newtype PSString = PSString Text
  deriving (Eq, Ord, Semigroup, Monoid, Generic)

instance NFData PSString
instance Serialise PSString

instance Show PSString where
  show (PSString t) = show (T.unpack t)

-- | Text cannot represent an unpaired UTF-16 surrogate; replace any such
-- code point with U+FFFD REPLACEMENT CHARACTER.
sanitize :: Text -> Text
sanitize = T.map $ \c -> if c >= '\xD800' && c <= '\xDFFF' then '\xFFFD' else c

mkString :: Text -> PSString
mkString = PSString . sanitize

fromText :: Text -> PSString
fromText = mkString

instance IsString PSString where
  fromString = mkString . T.pack

-- |
-- Encode a Char as one or two UTF-16 code units, using a surrogate pair for
-- characters outside the Basic Multilingual Plane.
--
charToUTF16 :: Char -> [Word16]
charToUTF16 c
  | n > 0xFFFF =
      let n' = n - 0x10000
      in [ fromIntegral (0xD800 + (n' `shiftR` 10))
         , fromIntegral (0xDC00 + (n' .&. 0x3FF))
         ]
  | otherwise = [fromIntegral n]
  where n = Char.ord c

toUTF16CodeUnits :: PSString -> [Word16]
toUTF16CodeUnits (PSString t) = concatMap charToUTF16 (T.unpack t)

-- |
-- Decode a PSString as text. Always succeeds now that PSString is backed by
-- Text; kept returning Maybe for source compatibility with existing callers.
--
decodeString :: PSString -> Maybe Text
decodeString (PSString t) = Just t

decodeStringEither :: PSString -> [Either Word16 Char]
decodeStringEither (PSString t) = map Right (T.unpack t)

decodeStringWithReplacement :: PSString -> String
decodeStringWithReplacement (PSString t) = T.unpack t

instance A.ToJSON PSString where
  toJSON (PSString t) = A.toJSON t

instance A.FromJSON PSString where
  parseJSON a = mkString <$> A.parseJSON a

-- |
-- Pretty print a PSString, using PureScript escape sequences.
--
prettyPrintString :: PSString -> Text
prettyPrintString (PSString t) = "\"" <> T.concatMap encodeChar t <> "\""
  where
  encodeChar :: Char -> Text
  encodeChar c
    | c == '\t' = "\\t"
    | c == '\r' = "\\r"
    | c == '\n' = "\\n"
    | c == '"'  = "\\\""
    | c == '\'' = "\\\'"
    | c == '\\' = "\\\\"
    | shouldPrint c = T.singleton c
    | otherwise = "\\x" <> showHex' 6 (Char.ord c)

  -- Note we do not use Data.Char.isPrint here because that includes things
  -- like zero-width spaces and combining punctuation marks, which could be
  -- confusing to print unescaped.
  shouldPrint :: Char -> Bool
  -- The standard space character, U+20 SPACE, is the only space char we should
  -- print without escaping
  shouldPrint ' ' = True
  shouldPrint c =
    Char.generalCategory c `elem`
      [ Char.UppercaseLetter
      , Char.LowercaseLetter
      , Char.TitlecaseLetter
      , Char.OtherLetter
      , Char.DecimalNumber
      , Char.LetterNumber
      , Char.OtherNumber
      , Char.ConnectorPunctuation
      , Char.DashPunctuation
      , Char.OpenPunctuation
      , Char.ClosePunctuation
      , Char.InitialQuote
      , Char.FinalQuote
      , Char.OtherPunctuation
      , Char.MathSymbol
      , Char.CurrencySymbol
      , Char.ModifierSymbol
      , Char.OtherSymbol
      ]

-- |
-- Pretty print a PSString, using JavaScript escape sequences. Intended for
-- use in compiled JS output.
--
prettyPrintStringJS :: PSString -> Text
prettyPrintStringJS s = "\"" <> foldMap encodeChar (toUTF16CodeUnits s) <> "\""
  where
  encodeChar :: Word16 -> Text
  encodeChar c | c > 0xFF = "\\u" <> showHex' 4 c
  encodeChar c | c > 0x7E || c < 0x20 = "\\x" <> showHex' 2 c
  encodeChar c | toChar c == '\b' = "\\b"
  encodeChar c | toChar c == '\t' = "\\t"
  encodeChar c | toChar c == '\n' = "\\n"
  encodeChar c | toChar c == '\v' = "\\v"
  encodeChar c | toChar c == '\f' = "\\f"
  encodeChar c | toChar c == '\r' = "\\r"
  encodeChar c | toChar c == '"'  = "\\\""
  encodeChar c | toChar c == '\\' = "\\\\"
  encodeChar c = T.singleton $ toChar c

toChar :: Word16 -> Char
toChar = toEnum . fromIntegral

showHex' :: Enum a => Int -> a -> Text
showHex' width c =
  let hs = showHex (fromEnum c) "" in
  T.pack (replicate (width - length hs) '0' <> hs)
