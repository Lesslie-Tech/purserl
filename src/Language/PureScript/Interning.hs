{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- A process-wide string-interning pass, applied to a freshly-decoded
-- 'Language.PureScript.Externs.ExternsFile'.
--
-- Loading a real project's externs (e.g. under @purs ide server@, which
-- retains every module's 'Language.PureScript.Externs.ExternsFile'
-- simultaneously) heap-profiles as dominated by boxed 'Text' values, almost
-- all of which are duplicates: common names like module names, "Prim",
-- "Type", "Maybe" etc. get their own independent heap allocation every time
-- they're deserialised, once per module that mentions them. 'intern'
-- rewrites a value so that every 'Text' leaf is canonicalized against a
-- single shared table, so repeated occurrences of the same string content
-- share one underlying heap object instead of each paying for their own.
--
-- The traversal is derived generically via 'GHC.Generics': every
-- PureScript-internal type already derives 'Generic' (it's needed for the
-- existing 'Codec.Serialise.Serialise' instances), so `instance Intern Foo`
-- is an empty, structurally-derived instance for nearly every compiler type
-- -- one is declared next to each type's definition, the same way
-- `instance NFData Foo`/`instance Serialise Foo` already are. Only the
-- actual leaf (Text), primitives with no Text inside, and a handful of
-- container shapes need a hand-written case, all of which live here.
module Language.PureScript.Interning
  ( Intern(..)
  , GIntern
  ) where

import Prelude

import Data.HashMap.Strict qualified as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

-- | A single, process-lifetime intern table, the same way
-- "Language.PureScript.Externs" already caches env-var lookups via a
-- top-level 'unsafePerformIO'. There's exactly one of these per process, and
-- it's safe to share across concurrent rebuilds (e.g. under @purs ide
-- server@).
internTable :: IORef (HM.HashMap Text Text)
internTable = unsafePerformIO (newIORef HM.empty)
{-# NOINLINE internTable #-}

-- | Canonicalize a 'Text' value against the shared table. The result is
-- always `== ` the input; only its heap identity may differ.
--
-- Once the table is warm, almost every call is a hit (a small number of
-- distinct names get looked up over and over across a whole project's
-- externs), so the hit path is a plain unsynchronized 'readIORef' -- no
-- point paying for 'atomicModifyIORef''s CAS-retry loop just to observe the
-- table without changing it. Only a genuine miss falls through to the
-- atomic path, re-checking under it in case another thread inserted the
-- same key in the meantime.
internText :: Text -> Text
internText t = unsafePerformIO $ do
  m <- readIORef internTable
  case HM.lookup t m of
    Just t' -> pure t'
    Nothing ->
      atomicModifyIORef' internTable $ \m' ->
        case HM.lookup t m' of
          Just t' -> (m', t')
          Nothing -> (HM.insert t t m', t)
{-# NOINLINE internText #-}

class Intern a where
  intern :: a -> a
  default intern :: (Generic a, GIntern (Rep a)) => a -> a
  intern = to . gintern . from

class GIntern f where
  gintern :: f p -> f p

instance GIntern V1 where
  gintern = id

instance GIntern U1 where
  gintern = id

instance (GIntern f, GIntern g) => GIntern (f :+: g) where
  gintern (L1 x) = L1 (gintern x)
  gintern (R1 x) = R1 (gintern x)

instance (GIntern f, GIntern g) => GIntern (f :*: g) where
  gintern (x :*: y) = gintern x :*: gintern y

instance GIntern f => GIntern (M1 i c f) where
  gintern (M1 x) = M1 (gintern x)

instance Intern c => GIntern (K1 i c) where
  gintern (K1 x) = K1 (intern x)

-- The one real leaf.
instance Intern Text where
  intern = internText

-- Primitives with no Text inside -- no Generic instance to derive from, so
-- these need a manual no-op.
instance Intern Int where intern = id
instance Intern Integer where intern = id
instance Intern Word where intern = id
instance Intern Bool where intern = id
instance Intern Char where intern = id
instance Intern Double where intern = id
instance Intern () where intern = id

-- Container shapes not already covered by a derived Generic instance.
instance Intern a => Intern [a] where
  intern = map intern

instance (Intern k, Intern v, Ord k) => Intern (M.Map k v) where
  intern = M.fromList . map intern . M.toList

instance (Intern a, Ord a) => Intern (Set a) where
  intern = Set.fromList . map intern . Set.toList

instance Intern a => Intern (NonEmpty a) where
  intern = fmap intern

instance Intern a => Intern (Maybe a) where
  intern = fmap intern

instance (Intern a, Intern b) => Intern (Either a b) where
  intern (Left a) = Left (intern a)
  intern (Right b) = Right (intern b)

instance (Intern a, Intern b) => Intern (a, b) where
  intern (a, b) = (intern a, intern b)

instance (Intern a, Intern b, Intern c) => Intern (a, b, c) where
  intern (a, b, c) = (intern a, intern b, intern c)
