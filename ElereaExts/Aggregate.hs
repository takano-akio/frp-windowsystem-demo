{-# OPTIONS_GHC -Wall #-}
-- Dynamically connected signals

module ElereaExts.Aggregate
  ( Aggregator
  , SignalGenA
  , aggregate
  , connect
  , runSignalGenA
  ) where

import ElereaExts.MonadSignalGen

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Maybe
import qualified Data.Vault as V

-- | A variant of @SignalGen@ that supports two additional operations,
-- @aggregate@ and @connect@.
newtype SignalGenA a = SG { unSG :: Bus -> Bus -> SignalGen (Bus, a) }

type Bus = Signal V.Vault

instance Monad SignalGenA where
  return x = SG $ \_ bus -> return (bus, x)
  SG x >>= f = SG $ \env bus -> do
    (bus1, v) <- x env bus
    unSG (f v) env bus1

instance Functor SignalGenA where
  fmap = liftM

instance Applicative SignalGenA where
  pure = return
  (<*>) = ap

instance MonadFix SignalGenA where
  mfix f = SG $ \env bus ->
    mfix $ \ ~(_, r) -> unSG (f r) env bus

instance MonadSignalGen SignalGenA where
  liftSignalGen sg = SG $ \_ bus -> do
    r <- sg
    return (bus, r)
  -- The generator signal must not directly depend on any aggregated
  -- signal because that would cause a circular dependency.
  -- Sometimes you can use @liftSignalGen . generator@ instead
  -- to work around the problem.
  generator generatorSig = SG $ \env bus -> do
    tupleSig <- generator $ unlift env bus <$> generatorSig
    return (join $ fst <$> tupleSig, snd <$> tupleSig)
    where
      unlift env bus gen = unSG gen env bus

-- | A sink of information to which signals can be connected.
newtype Aggregator a = Aggregator (V.Key [a])

-- | Create an @Aggregator@. It returns a signal that collects
-- the signals connected to the Aggregator.
aggregate :: SignalGenA (Signal [a], Aggregator a)
aggregate = do
  key <- execute V.newKey
  sig <- extract key
  return (sig, Aggregator key)

addToBus :: V.Key [a] -> Signal a -> SignalGenA ()
addToBus key sig = SG $ \_ bus -> return (upd <$> bus <*> sig, ())
  where
    upd univ val = case V.lookup key univ of
      Just x -> V.insert key (val:x) univ
      _ -> V.insert key [val] univ

extract :: V.Key [a] -> SignalGenA (Signal [a])
extract key = SG $ \env bus -> return (bus, f <$> env)
  where
    f univ = fromMaybe [] $ V.lookup key univ

-- | Connect a signal to an aggregator. They will be kept connected
-- while the current subnetwork is alive.
connect :: Aggregator a -> Signal a -> SignalGenA ()
connect (Aggregator key) sig = addToBus key sig

-- | Run the @SignalGenA@ monad inside the @SignalGen@ monad.
runSignalGenA :: SignalGenA a -> SignalGen a
runSignalGenA (SG x) = do
  (_, val) <- mfix $ \ ~(env, _) -> x env (pure V.empty)
  return val
