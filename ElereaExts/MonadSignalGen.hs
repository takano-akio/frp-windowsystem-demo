{-# OPTIONS_GHC -Wall #-}
-- | Polymorphic operations defined over any SignalGen-like monads.
module ElereaExts.MonadSignalGen
  ( MonadSignalGen(..)

  -- lifted version of elerea combinators
  , externalMulti
  , delay
  , snapshot
  , memo
  , until
  , stateful
  , transfer
  -- not implemented yet
  --, transfer2
  --, transfer3
  --, transfer4
  , execute
  , effectful
  , effectful1
  --, effectful2
  --, effectful3
  --, effectful4
  
  -- re-exports
  , E.external
  , Signal
  , SignalGen
  ) where

import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Simple (Signal, SignalGen)
import qualified FRP.Elerea.Simple as E
import Prelude hiding (until)

-- | A class of monads that can be used like the @SignalGen@ monad
class (MonadFix m, Applicative m) => MonadSignalGen m where
  liftSignalGen :: SignalGen a -> m a
  generator :: Signal (m a) -> m (Signal a)

instance MonadSignalGen SignalGen where
  liftSignalGen = id
  generator = E.generator

externalMulti :: (MonadSignalGen m) => IO (m (Signal [a]), a -> IO ())
externalMulti = do
  (gen, trigger) <- E.externalMulti
  return (liftSignalGen gen, trigger)

delay :: (MonadSignalGen m) => a -> Signal a -> m (Signal a)
delay a b = liftSignalGen $ E.delay a b

snapshot :: (MonadSignalGen m) => Signal a -> m a
snapshot a = liftSignalGen $ E.snapshot a

memo :: (MonadSignalGen m) => Signal a -> m (Signal a)
memo a = liftSignalGen $ E.memo a

until :: (MonadSignalGen m) => Signal Bool -> m (Signal Bool)
until a = liftSignalGen $ E.until a

stateful :: (MonadSignalGen m) => a -> (a -> a) -> m (Signal a)
stateful a b = liftSignalGen $ E.stateful a b

transfer :: (MonadSignalGen m) => a -> (t -> a -> a) -> Signal t -> m (Signal a)
transfer a b c = liftSignalGen $ E.transfer a b c

execute :: (MonadSignalGen m) => IO a -> m a
execute a = liftSignalGen $ E.execute a

effectful :: (MonadSignalGen m) => IO a -> m (Signal a)
effectful a = liftSignalGen $ E.effectful a

effectful1 :: (MonadSignalGen m) => (t -> IO a) -> Signal t -> m (Signal a)
effectful1 a b = liftSignalGen $ E.effectful1 a b
