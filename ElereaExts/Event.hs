{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, DeriveDataTypeable #-}

{-# OPTIONS_GHC -Wall #-}
-- | Event/discrete layer constructed on top of Elera.
-- The API is largely inspired by reactive-banana.
module ElereaExts.Event where

import Control.Applicative
import Control.Monad.Fix
import Data.List (foldl')
import Data.Monoid
import Data.Maybe
import Data.Typeable

import ElereaExts.MonadSignalGen

newtype Event a = Event (Signal [a])
  deriving (Functor, Typeable)
newtype Discrete a = Discrete (Signal (Bool, a))
  -- The first component indicates if the value may be new.
  -- If it is False, the consumer should avoid evaluating the
  -- second component whenever possible.
  -- FIXME: This trick alone cannot remove all redundant recomputations.
  -- Consider the case where a Discrete is
  -- read every iteration in a fresh SignalGen run.
  deriving (Functor, Typeable)
-- type Behavior a = Signal a

instance Monoid (Event a) where
  mempty = Event $ pure []
  Event a `mappend` Event b = Event $ (++) <$> a <*> b

infixl 4 <@>, <@

class (Functor f, Functor g) => Apply f g where
  (<@>) :: f (a -> b) -> g a -> g b
  (<@) :: f a -> g b -> g a

  f <@ g = const <$> f <@> g

instance Apply Signal Event where
  (<@>) = apply

instance Apply Signal Discrete where
  x <@> y = signalToDiscrete x <*> y

-- It's difficult to implement this without causing needless recalculation:
--instance Apply Discrete Event where

externalEvent :: (MonadSignalGen m) => IO (m (Event a), a -> IO ())
externalEvent = do
  (gen, trigger) <- externalMulti
  return (Event <$> gen, trigger)

apply :: Signal (a -> b) -> Event a -> Event b
apply sig (Event evt) = Event $ map <$> sig <*> evt

filterE :: (a -> Bool) -> Event a -> Event a
filterE cond (Event evt) = Event $ filter cond <$> evt

stepper :: (MonadSignalGen m) => a -> Event a -> m (Signal a)
stepper initial (Event evt) = transfer initial upd evt
  where
    upd [] old = old
    upd occs _ = last occs

eachSample :: Signal a -> Event a
eachSample = Event . fmap (:[])

eachSampleD :: (MonadSignalGen m) => Discrete a -> m (Event a)
eachSampleD d = do
  sig <- discreteToSignal d
  return $ eachSample sig

accumB :: (MonadSignalGen m) => a -> Event (a -> a) -> m (Signal a)
accumB initial (Event evt) = transfer initial upd evt
  where
    upd occs old = foldl' (flip ($)) old occs

accumBIO :: (MonadSignalGen m) => a -> Event (a -> IO a) -> m (Signal a)
accumBIO initial (Event evt) = mfix $ \self -> do
  prev <- delay initial self
  effectful1 id $ update <$> prev <*> evt
  where
    update prev upds = foldl' (>>=) (return prev) upds

accumE :: (MonadSignalGen m) => a -> Event (a -> a) -> m (Event a)
accumE initial (Event evt) = fmap Event $ do
  (_, occs) <- mfix $ \ ~(self, _) -> do
    prev <- delay initial self
    vs <- memo $ scanl (flip ($)) <$> prev <*> evt
    return (last <$> vs, tail <$> vs)
  return occs

flattenE :: Event [a] -> Event a
flattenE (Event evt) = Event $ concat <$> evt

effectfulEE :: (MonadSignalGen m) => (t -> IO a) -> Event t -> m (Event a)
effectfulEE mkAction (Event evt) = Event <$> effectful1 (mapM mkAction) evt

memoE :: (MonadSignalGen m) => Event a -> m (Event a)
memoE (Event evt) = Event <$> memo evt

joinEventSignal :: Signal (Event a) -> Event a
joinEventSignal sig = Event $ do
  Event occs <- sig
  occs

filterNothingE :: Event (Maybe a) -> Event a
filterNothingE (Event evt) = Event $ catMaybes <$> evt

mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f evt = filterNothingE $ f <$> evt

onCreation :: (MonadSignalGen m) => a -> m (Event a)
onCreation x = Event <$> delay [x] (return [])

delayE :: (MonadSignalGen m) => Event a -> m (Event a)
delayE (Event x) = Event <$> delay [] x

eventToRef :: Event () -> Signal ()
eventToRef (Event x) = (const ()) <$> x

withPrev :: (MonadSignalGen m) => a -> Event a -> m (Event (a, a))
withPrev initial evt = accumE (initial, undefined) $ toUpd <$> evt
  where
    toUpd val (new, _old) = (val, new)

generatorE :: (MonadSignalGen m) => Event (m a) -> m (Event a)
generatorE (Event evt) = Event <$> generator (sequence <$> evt)

eventToSignal :: Event a -> Signal [a]
eventToSignal (Event x) = x

signalToEvent :: Signal [a] -> Event a
signalToEvent = Event

changesD :: Discrete a -> Event a
changesD (Discrete dis) = Event $ conv <$> dis
  where
    conv (new, x) = if new then [x] else []

stepperD :: (MonadSignalGen m) => a -> Event a -> m (Discrete a)
stepperD initial (Event evt) = Discrete <$> transfer (False, initial) upd evt
  where
    upd [] (_, old) = (False, old)
    upd occs _ = (True, last occs)

accumD :: (MonadSignalGen m) => a -> Event (a -> a) -> m (Discrete a)
accumD initial (Event evt) = Discrete <$> transfer (False, initial) upd evt
  where
    upd [] (_, old) = (False, old)
    upd upds (_, old) = (True, new)
      where new = foldl' (flip ($)) old upds

instance Applicative Discrete where
  pure x = Discrete $ pure (False, x)
  Discrete f <*> Discrete a = Discrete $ app <$> f <*> a
    where
      app (newFun, fun) (newArg, arg) = (new, fun arg)
        where !new = newFun || newArg

instance Monad Discrete where
  return x = Discrete $ return (False, x)
  Discrete x >>= f = Discrete $ do
    (newX, v) <- x
    let Discrete y = f v
    (newY, r) <- y
    let !new = newX || newY
    return (new, r)

memoD :: (MonadSignalGen m) => Discrete a -> m (Discrete a)
memoD (Discrete dis) = Discrete <$> memo dis

delayD :: (MonadSignalGen m) => a -> Discrete a -> m (Discrete a)
delayD initial (Discrete subsequent) = Discrete <$> delay (True, initial) subsequent

minimizeChanges :: (MonadSignalGen m, Eq a) => Discrete a -> m (Discrete a)
minimizeChanges (Discrete dis) = Discrete . fmap fromJust <$> transfer Nothing upd dis
  where
    upd (False, _) (Just (_, cache)) = Just (False, cache)
    upd (True, val) (Just (_, cache))
      | val == cache = Just (False, cache)
    upd (new, val) _ = Just (new, val)

discreteToSignal :: (MonadSignalGen m) => Discrete a -> m (Signal a)
discreteToSignal (Discrete dis) = fmap fromJust <$> transfer Nothing upd dis
  where
    upd (False, _) (Just cache) = Just cache
    upd (_, val) _ = Just val

signalToDiscrete :: Signal a -> Discrete a
signalToDiscrete x = Discrete $ (,) True <$> x
