{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-- |
-- Functions for replacing fully applied type synonyms
--
module Language.PureScript.TypeChecker.Synonyms
  ( SynonymMap
  , KindMap
  , replaceAllTypeSynonyms
  ) where

import Prelude

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State (MonadState)
import Data.Maybe (fromMaybe)
import Data.Map qualified as M
import Data.Text (Text)
import Language.PureScript.Environment (Environment(..), TypeKind)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), SourceSpan, errorMessage', safst)
import Language.PureScript.Names (ProperName, ProperNameType(..), Qualified)
import Language.PureScript.TypeChecker.Monad (Check, CheckState, getEnv)
import Language.PureScript.Types (SourceType, Type(..), completeBinderList, getAnnForType, mapTypeChildrenSharedM, ptrEq, replaceAllTypeVars)

-- | Type synonym information (arguments with kinds, aliased type), indexed by name
type SynonymMap = M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe SourceType)], SourceType)

type KindMap = M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)

replaceAllTypeSynonyms'
  :: SynonymMap
  -> KindMap
  -> SourceType
  -> Either MultipleErrors SourceType
replaceAllTypeSynonyms' syns kinds = top
  where
  -- Top-down, sharing-preserving. A spine of applications can only be rewritten
  -- (or rejected) through its head, so a spine whose head is not a synonym is
  -- traversed for its arguments alone, without looking its head up per node.
  top :: SourceType -> Either MultipleErrors SourceType
  top t = case t of
    TypeApp{} -> spine t
    KindApp{} -> spine t
    TypeConstructor{} -> spine t
    _ -> mapTypeChildrenSharedM top t

  spine :: SourceType -> Either MultipleErrors SourceType
  spine t = case spineHead t of
    TypeConstructor _ ctor | M.member ctor syns -> try t >>= mapTypeChildrenSharedM top
    _ -> plain t

  spineHead :: SourceType -> SourceType
  spineHead (TypeApp _ f _) = spineHead f
  spineHead (KindApp _ f _) = spineHead f
  spineHead t = t

  plain :: SourceType -> Either MultipleErrors SourceType
  plain t = case t of
    TypeApp ann f a -> do
      !f' <- plain f
      !a' <- top a
      pure $! if ptrEq f' f && ptrEq a' a then t else TypeApp ann f' a'
    KindApp ann f k -> do
      !f' <- plain f
      !k' <- top k
      pure $! if ptrEq f' f && ptrEq k' k then t else KindApp ann f' k'
    TypeConstructor{} -> pure t
    _ -> top t

  try :: SourceType -> Either MultipleErrors SourceType
  try t = fromMaybe t <$> go (safst $ getAnnForType t) 0 [] [] t

  go :: SourceSpan -> Int -> [SourceType] -> [SourceType] -> SourceType -> Either MultipleErrors (Maybe SourceType)
  go ss c kargs args (TypeConstructor _ ctor)
    | Just (synArgs, body) <- M.lookup ctor syns
    , c == length synArgs
    , kindArgs <- lookupKindArgs ctor
    , length kargs == length kindArgs
    = let repl = replaceAllTypeVars (zip (map fst synArgs) args <> zip kindArgs kargs) body
      in Just <$> try repl
    | Just (synArgs, _) <- M.lookup ctor syns
    , length synArgs > c
    = throwError . errorMessage' ss $ PartiallyAppliedSynonym ctor
  go ss c kargs args (TypeApp _ f arg) = go ss (c + 1) kargs (arg : args) f
  go ss c kargs args (KindApp _ f arg) = go ss c (arg : kargs) args f
  go _ _ _ _ _ = return Nothing

  lookupKindArgs :: Qualified (ProperName 'TypeName) -> [Text]
  lookupKindArgs ctor = fromMaybe [] $ fmap (fmap (fst . snd) . fst) . completeBinderList . fst =<< M.lookup ctor kinds

-- | Replace fully applied type synonyms
replaceAllTypeSynonyms :: SourceType -> Check SourceType
replaceAllTypeSynonyms d = do
  env <- getEnv
  either throwError return $ replaceAllTypeSynonyms' (typeSynonyms env) (types env) d
