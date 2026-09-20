{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.Erl.Errors
  ( module Language.PureScript.Erl.Errors
  ) where

import Prelude

import           Control.Monad.Error.Class (MonadError(..))
import           Language.PureScript.AST (ErrorMessageHint(..))
import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Erl.Errors.Types
    ( ErrorMessage(..), SimpleErrorMessage(..) )
-- purserl

-- | A stack trace for an error
newtype MultipleErrors = MultipleErrors
  { runMultipleErrors :: [ErrorMessage]
  } deriving (Show, Semigroup, Monoid)

-- | Create an error set from a single simple error message
errorMessage :: SimpleErrorMessage -> MultipleErrors
errorMessage err = MultipleErrors [ErrorMessage [] err]

-- | Lift a function on ErrorMessage to a function on MultipleErrors
onErrorMessages :: (ErrorMessage -> ErrorMessage) -> MultipleErrors -> MultipleErrors
onErrorMessages f = MultipleErrors . map f . runMultipleErrors

-- | Add a hint to an error message
addHint :: ErrorMessageHint -> MultipleErrors -> MultipleErrors
addHint hint = addHints [hint]

-- | Add hints to an error message
addHints :: [ErrorMessageHint] -> MultipleErrors -> MultipleErrors
addHints hints = onErrorMessages $ \(ErrorMessage hints' se) -> ErrorMessage (hints ++ hints') se

-- | Rethrow an error with a more detailed error message in the case of failure
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError (throwError . f)

-- | Rethrow an error with source position information
rethrowWithPosition :: (MonadError MultipleErrors m) => SourceSpan -> m a -> m a
rethrowWithPosition pos = rethrow (onErrorMessages (withPosition pos))

withPosition :: SourceSpan -> ErrorMessage -> ErrorMessage
withPosition NullSourceSpan err = err
withPosition pos (ErrorMessage hints se) = ErrorMessage (positionedError pos : hints) se

positionedError :: SourceSpan -> ErrorMessageHint
positionedError = PositionedError . pure
