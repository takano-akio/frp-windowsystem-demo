{-# OPTIONS_GHC -Wall #-}
-- Dynamically connected events

module ElereaExts.EventAggregate
  ( AggregatorE
  , aggregateE
  , connectE
  , sendE
  ) where

import ElereaExts.Aggregate
import ElereaExts.Event

newtype AggregatorE a = AE (Aggregator [a])

aggregateE :: SignalGenA (Event a, AggregatorE a)
aggregateE = do 
  (sig, aggr) <- aggregate
  evt <- memoE $ flattenE $ signalToEvent sig
  return (evt, AE aggr)

connectE :: AggregatorE a -> Event a -> SignalGenA ()
connectE (AE aggr) evt = connect aggr (eventToSignal evt)

sendE :: AggregatorE a -> a -> SignalGenA ()
sendE ae val = do
  singleton <- onCreation val
  connectE ae singleton
