module Util where

import Control.Applicative
import Data.Monoid
import Debug.Trace
import FRP.Elerea.Simple (Signal)

newtype Task m = Task { runTask :: m () }

instance (Monad m) => Monoid (Task m) where
  mempty = Task $ return ()
  Task x `mappend` Task y = Task $ x >> y

traceF :: (Show a, Functor f) => String -> f a -> f a
traceF loc = fmap (\val -> trace (loc ++ ": " ++ show val) val)

-- orphan, but so useful
instance (Monoid a) => Monoid (Signal a) where
  mempty = pure mempty
  mappend = liftA2 mappend
  mconcat = fmap mconcat . sequence
