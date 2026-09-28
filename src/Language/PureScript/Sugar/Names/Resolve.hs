-- |
-- A minimal, self-contained monad for the name-resolution logic shared
-- between the desugaring pipeline ('Language.PureScript.Sugar.Monad.DesugarM')
-- and 'Language.PureScript.Make'\'s extern-environment construction. That
-- logic only ever needs error and warning accumulation, never the extra
-- state/supply capabilities either ambient monad carries -- so it's
-- concretized here instead of staying polymorphic, and each ambient monad
-- bridges into it with 'runResolveM'.
--
module Language.PureScript.Sugar.Names.Resolve (ResolveM, runResolveM) where

import Prelude

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Writer (Writer, runWriter, MonadWriter(..))

import Language.PureScript.Errors (MultipleErrors)

type ResolveM = ExceptT MultipleErrors (Writer MultipleErrors)

runResolveM :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => ResolveM a -> m a
runResolveM m = case runWriter (runExceptT m) of
  (Right a, w) -> tell w >> pure a
  (Left e, w)  -> tell w >> throwError e
