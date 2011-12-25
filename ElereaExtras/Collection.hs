{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wall #-}
-- | Collection signals
module ElereaExtras.Collection
  ( Collection
  , CollectionDelta(..)
  , memoCollection
  , collectionChanges
  , collectionToMap
  , subcollection
  , mapSignalToSignalCollection
  , deltaEventToCollection
  , singletonCollection
  , traceCollection
  ) where

import Control.Applicative
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import ElereaExtras.MonadSignalGen
import ElereaExtras.Event

-- | @Collection k a@ is like @Signal (M.Map k a)@, but it carries the
-- extra information about when the map changes, i.e. some element is
-- added/removed. This information can be extracted using @collectionChanges@.
newtype Collection k a = Col (Signal ([CollectionDelta k a], M.Map k a))
  deriving (Functor)

data CollectionDelta k a
  = Add k a
  | Remove k
  deriving (Show, Functor)

memoCollection :: (MonadSignalGen m) => Collection k a -> m (Collection k a)
memoCollection (Col x) = Col <$> memo x

collectionChanges :: Collection k a -> Event (CollectionDelta k a)
collectionChanges (Col col) = Event $ fst <$> col

collectionToMap :: Collection k a -> Signal (M.Map k a)
collectionToMap (Col col) = snd <$> col

singletonCollection :: (Ord k, MonadSignalGen m) => k -> a -> m (Collection k a)
singletonCollection key val = onCreation (Add key val) >>= deltaEventToCollection

subcollection :: (Ord k, Ord l) => (k -> Maybe l) -> Collection k a -> Collection l a
subcollection f (Col col) = Col (trans <$> col)
  where
    trans (deltas, m) = (mapMaybe transDelta deltas, submap m) 
    transDelta (Add key val) = Add <$> f key <*> pure val
    transDelta (Remove key) = Remove <$> f key
    submap m = M.fromList $ mapMaybe transEntry $ M.toList m
    transEntry (key, val) = (,) <$> f key <*> pure val

mapSignalToSignalCollection
  :: (MonadSignalGen m, Ord k)
  => Discrete (M.Map k a)
  -> m (Collection k (Discrete a))
mapSignalToSignalCollection input =
  memoD input
  >>= mapSignalToSignalDeltaEvent
  >>= deltaEventToCollection

mapSignalToSignalDeltaEvent
  :: (MonadSignalGen m, Ord k)
  => Discrete (M.Map k a)
  -> m (Event (CollectionDelta k (Discrete a)))
mapSignalToSignalDeltaEvent m = do
  keyChange <- withPrev S.empty (changesD $ M.keysSet <$> m)
  memoE $ flattenE $ keyChangeToDeltas <$> keyChange
  where
    keyChangeToDeltas (keys, prevKeys) = removedList ++ addedList
      where
        removedList = map Remove $ S.toList $ (S.difference prevKeys keys)
        addedList = map mkAdd $ S.toList $ (S.difference keys prevKeys)
    mkAdd k = Add k ((M.!) <$> m <*> pure k)

deltaEventToCollection
  :: (MonadSignalGen m, Ord k)
  => Event (CollectionDelta k a)
  -> m (Collection k a)
deltaEventToCollection (Event evt) = Col <$> transfer ([], M.empty) upd evt
  where
    upd deltas (_, m) = (deltas, m')
      where
        !m' = foldl' applyDelta m deltas
    applyDelta m (Add k a) = M.insert k a m
    applyDelta m (Remove k) = M.delete k m

traceCollection :: String -> Collection k a -> Collection k a
traceCollection loc (Col sig) = Col (traceSignalMaybe loc f sig)
  where
    f ([], _) = Nothing
    f (chs, _) = Just $ unwords $ map to_s chs
    to_s Add{} = "add"
    to_s Remove{} = "remove"
