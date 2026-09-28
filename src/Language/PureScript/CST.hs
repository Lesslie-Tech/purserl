module Language.PureScript.CST
  ( parseFromFile
  , parseFromFiles
  , parseModuleFromFile
  , parseModulesFromFiles
  , unwrapParserError
  , toMultipleErrors
  , toMultipleWarnings
  , toPositionedError
  , toPositionedWarning
  , pureResult
  , module Language.PureScript.CST.Convert
  , module Language.PureScript.CST.Errors
  , module Language.PureScript.CST.Lexer
  , module Language.PureScript.CST.Monad
  , module Language.PureScript.CST.Parser
  , module Language.PureScript.CST.Print
  , module Language.PureScript.CST.Types
  ) where

import Prelude hiding (lex)

import Control.Monad.Error.Class (MonadError(..))
import Control.Parallel.Strategies (withStrategy, parList, evalTuple2, r0, rseq)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Language.PureScript.AST qualified as AST
import Language.PureScript.Errors qualified as E
import Language.PureScript.CST.Convert
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Lexer
import Language.PureScript.CST.Monad (Parser, ParserM(..), ParserState(..), LexResult, runParser, runTokenParser)
import Language.PureScript.CST.Parser
import Language.PureScript.CST.Print
import Language.PureScript.CST.Types

pureResult :: a -> PartialResult a
pureResult a = PartialResult a ([], pure a)

parseModulesFromFiles
  :: forall k
   . (k -> FilePath)
  -> [(k, Text)]
  -> Either E.MultipleErrors [(k, PartialResult AST.Module)]
parseModulesFromFiles toFilePath input =
  flip E.parU (handleParserError toFilePath)
    . inParallel
    . flip fmap input
    $ \(k, a) -> (k, parseModuleFromFile (toFilePath k) a)

parseFromFiles
  :: forall k
   . (k -> FilePath)
  -> [(k, Text)]
  -> Either E.MultipleErrors [(k, ([ParserWarning], AST.Module))]
parseFromFiles toFilePath input =
  flip E.parU (handleParserError toFilePath)
    . inParallel
    . flip fmap input
    $ \(k, a) -> (k, sequence $ parseFromFile (toFilePath k) a)

parseModuleFromFile :: FilePath -> Text -> Either (NE.NonEmpty ParserError) (PartialResult AST.Module)
parseModuleFromFile fp content = fmap (convertModule (T.pack fp)) <$> parseModule (lexModule content)

parseFromFile :: FilePath -> Text -> ([ParserWarning], Either (NE.NonEmpty ParserError) AST.Module)
parseFromFile fp content = fmap (convertModule (T.pack fp)) <$> parse content

handleParserError
  :: forall k a
   . (k -> FilePath)
  -> (k, Either (NE.NonEmpty ParserError) a)
  -> Either E.MultipleErrors (k, a)
handleParserError toFilePath (k, res) =
  (k,) <$> unwrapParserError (toFilePath k) res

unwrapParserError
  :: forall a
   . FilePath
  -> Either (NE.NonEmpty ParserError) a
  -> Either E.MultipleErrors a
unwrapParserError fp =
  either (throwError . toMultipleErrors fp) pure

toMultipleErrors :: FilePath -> NE.NonEmpty ParserError -> E.MultipleErrors
toMultipleErrors fp =
  E.MultipleErrors . NE.toList . fmap (toPositionedError fp)

toMultipleWarnings :: FilePath -> [ParserWarning] -> E.MultipleErrors
toMultipleWarnings fp =
  E.MultipleErrors . fmap (toPositionedWarning fp)

toPositionedError :: FilePath -> ParserError -> E.ErrorMessage
toPositionedError name perr =
  E.ErrorMessage [E.positionedError $ sourceSpan (T.pack name) $ errRange perr] (E.ErrorParsingCSTModule perr)

toPositionedWarning :: FilePath -> ParserWarning -> E.ErrorMessage
toPositionedWarning name perr =
  E.ErrorMessage [E.positionedError $ sourceSpan (T.pack name) $ errRange perr] (E.WarningParsingCSTModule perr)

inParallel :: [(k, Either (NE.NonEmpty ParserError) a)] -> [(k, Either (NE.NonEmpty ParserError) a)]
inParallel = withStrategy (parList (evalTuple2 r0 rseq))
