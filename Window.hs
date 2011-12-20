{-# LANGUAGE DoRec, ExistentialQuantification, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
-- | A very simple window system, implemented on top of GLUT.
module Window where

import Draw
import GLUtil
import Util

import ElereaExts.Aggregate
import ElereaExts.Collection
import ElereaExts.Event
import ElereaExts.EventAggregate
import ElereaExts.MonadSignalGen

import Control.Applicative
import Control.Monad.RWS
import qualified Data.Map as M
import Data.List
import Data.Dynamic
import Data.Function
import Data.Maybe
import Data.Unique
import qualified Graphics.UI.GLUT as GL

type Window a = WindowInput -> SignalGenA (WindowOutput, a)
data WindowInput = WindowInput
  { wiKey :: (Event KeyEvent)
  , wiKeyState :: (Discrete KeyStateMap)
  , wiMetrics :: (Discrete WindowMetrics)
  , wiMetricsReq :: Discrete WindowMetrics
  , wiFocused :: (Discrete Bool)
  , wiCloseReq :: Event ()
  }
type WindowOutput = (Signal Draw, WindowMetrics, Discrete WindowMetrics)
type WindowMetrics = (Position, Size)
type GlobalInput = (Event KeyEvent, Discrete KeyStateMap, Signal Position)

-- | An abstract type representing an instance of the window system.
data WindowSystem = WS
  { wsBundleAggr :: AggregatorE WindowBundle
  , wsWindowOut :: Collection WindowKey Dynamic
  }
data WindowKey = forall k. (Typeable k, Ord k) => WindowKey !Unique !k
data WindowBundle = forall k a. (Typeable k, Ord k, Typeable a) =>
  WindowBundle !Unique (Collection k (Window a))

instance Eq WindowKey where
  x == y = compare x y == EQ

instance Ord WindowKey where
  compare (WindowKey u0 k0) (WindowKey u1 k1) = compare u0 u1 `mappend` keyOrder
    where
      keyOrder = case cast k0 of
        Just k -> compare k k1
        Nothing -> EQ

-- | Create an instance of the window system.
windowSystem :: GlobalInput -> SignalGenA (Signal Draw, WindowSystem)
windowSystem globalInput = do
  (bundleAdd, bundleAggr) <- aggregateE
  bundles <- accumB [] ((:) <$> bundleAdd)
  let delta = joinEventSignal $ mconcat . map bundleChanges <$> bundles
  windows <- deltaEventToCollection delta
  (draw, out) <- runWindows windows globalInput
  let ws = WS{ wsBundleAggr = bundleAggr, wsWindowOut = out }
  return (draw, ws)
  where
    bundleChanges (WindowBundle uniq col) = f <$> collectionChanges col
      where
        f (Add k w) = Add (WindowKey uniq k) (mapWindow toDyn w)
        f (Remove k) = Remove (WindowKey uniq k)

-- | Create a single window. The resulting window is never closed.
window :: (Typeable a) => WindowSystem -> Window a -> SignalGenA (Signal (Maybe a))
window sys w = generalWindow sys w mempty

-- | Create a single window. The window should return an event that
-- describes when to close it.
closableWindow
  :: (Typeable a)
  => WindowSystem
  -> Window (a, Event ())
  -> SignalGenA (Signal (Maybe a))
closableWindow sys w = do
  rec
    tupleSig <- generalWindow sys w close
    let close = joinEventSignal $ fromMaybe mempty . fmap snd <$> tupleSig
  return $ fmap fst <$> tupleSig

generalWindow
  :: (Typeable a)
  => WindowSystem
  -> Window a
  -> Event ()
  -> SignalGenA (Signal (Maybe a))
generalWindow sys w close = do
  add <- onCreation $ Add () w
  let remove = const (Remove ()) <$> close
  col <- deltaEventToCollection $ mappend add remove
  out <- manyWindows sys col
  memo $ M.lookup () <$> collectionToMap out

-- | Create multiple windows sharing an output type.
manyWindows :: (Typeable k, Ord k, Typeable a) =>
  WindowSystem -> Collection k (Window a) -> SignalGenA (Collection k a)
manyWindows WS{..} windowCol = do
  uniq <- execute newUnique
  sendE wsBundleAggr $ WindowBundle uniq windowCol
  --return undefined
  memoCollection $ undyn <$> subcollection (choose uniq) wsWindowOut
  where
    choose myUniq (WindowKey uniq key) = do
      guard $ uniq == myUniq
      cast key -- should always succeed
    undyn = fromMaybe (error "manyWindows: type mismatch") . fromDynamic

trigger :: AggregatorE a -> a -> SignalGenA ()
trigger aggr val = onCreation val >>= connectE aggr

-- | The low-level interface to the window system. You'll have to put
-- all windows together into a single collection to use this interface.
-- Also all the windows in the system must share an output type @a@.
runWindows
  :: (Ord k)
  => Collection k (Window a)
  -> GlobalInput
  -> SignalGenA (Signal Draw, Collection k a)
runWindows windowSpecs globalInput@(_, _, mousePos) = do
  (zMap, zMapUpdater) <- makeZMap
  prevZMap <- delay M.empty =<< discreteToSignal zMap
  rec
    prevFocus <- delay Nothing focus
    drag <- handleWindowDrag prevFocus globalInput
    windows <- deltaEventToCollection =<<
      handleWindowCreationDeletion prevFocus drag zMapUpdater
        (collectionChanges windowSpecs) globalInput
    sortedWindows <- memo $ sortWindows <$> prevZMap <*> collectionToMap windows
    focus <- findFocused mousePos sortedWindows
  _ <- generator $ whenJust <$> focus <*> pure zMapUpdater
  let
    draw = join $ mconcat . map (fmap layout . fst . snd) <$> sortedWindows
  return (draw, fmap snd windows)

sortWindows :: (Ord k) => M.Map k Int -> M.Map k v -> [(k, v)]
sortWindows zMap windows = sortBy (compare `on` zindex) $ M.toList windows
  where
    zindex (k, _) = M.lookup k zMap

makeZMap :: (Ord k) => SignalGenA (Discrete (M.Map k Int), k -> SignalGenA ())
makeZMap = do
  generationNo <- accumB 0 $ eachSample $ pure (+1)
  (zMap, aggr) <- newVariable M.empty
  let
    updater key = do
      no <- snapshot generationNo
      sendE aggr $ M.insert key no
  return (zMap, updater)

handleWindowDrag
  :: (Eq k)
  => Signal (Maybe k)
  -> GlobalInput
  -> SignalGenA (Event (k, Vector2 Int))
handleWindowDrag focus (keyEvt, _, mousePos) = do
  dragInstance <- stepper Nothing $ filterNothingE
    $ instanceChange <$> focus <@> keyEvt
  dragState <- memo $ fmap <$> ((,) <$> mousePos) <*> dragInstance
  prevState <- delay Nothing dragState
  memoE $ filterNothingE $ eachSample $ movement <$> dragState <*> prevState
  where
    instanceChange (Just wKey)
      (MouseButton LeftButton, GL.Down, GL.Modifiers{ GL.shift = GL.Down }, _)
      = Just $ Just wKey
    instanceChange _ (MouseButton LeftButton, GL.Up, _, _) = Just Nothing
    instanceChange _ _ = Nothing

    movement (Just (curPos, curWindow)) (Just (prevPos, prevWindow))
      | curWindow == prevWindow = Just (curWindow, relativePos prevPos curPos)
    movement _ _ = Nothing

    relativePos prevPos curPos = pos2vec curPos - pos2vec prevPos

findFocused
  :: Signal Position
  -> Signal [(k, (Signal (d, WindowMetrics), a))]
  -> SignalGenA (Signal (Maybe k))
findFocused mousePosition windows = 
  memo $ fmap fst <$> (finder <*> list)
  where
    list = do {-Signal-}
      ws <- windows
      forM ws $ \(key, (sig, _)) -> do
        (_, metrics) <- sig
        return (key, metrics)
    finder :: Signal ([(k, WindowMetrics)] -> Maybe (k, WindowMetrics))
    finder = do
      pos <- mousePosition
      return $ find (insideWindow pos . snd) . reverse

insideWindow :: Position -> WindowMetrics -> Bool
insideWindow (Position x y) (Position x0 y0, Size w h) =
  inside x0 w x && inside y0 h y
  where
    inside base rng t = base <= t && t <= base + fromIntegral rng

layout :: (Draw, WindowMetrics) -> Draw
layout (winDraw, (Position x y, _size)) =
  shift (fromIntegral x) (fromIntegral y) winDraw

handleWindowCreationDeletion
  :: (Eq k)
  => Signal (Maybe k)
  -> Event (k, Vector2 Int)
  -> (k -> SignalGenA ())
  -> Event (CollectionDelta k (Window a))
  -> GlobalInput
  -> SignalGenA (Event (CollectionDelta k (Signal (Draw, WindowMetrics), a)))
handleWindowCreationDeletion prevFocus moveRequest addToTop
    creationDeletion globalInput = do
  -- I have to use a delay here, otherwise I'll get a circular dependency.
  -- This means newly created windows do not appear until the next iteration.
  -- This is somewhat unsatisfactory, especially when your FRP program is
  -- driven by external events rather than a timer.
  ev <- delayE creationDeletion
  generatorE $ trans <$> ev
  where
    trans (Add key win) = do
      addToTop key
      entry <- createWindow key win prevFocus moveRequest globalInput
      return $ Add key entry
    trans (Remove key) = return $ Remove key

createWindow
  :: (Eq k)
  => k
  -> Window a
  -> Signal (Maybe k)
  -> Event (k, Vector2 Int)
  -> GlobalInput
  -> SignalGenA (Signal (Draw, WindowMetrics), a)
createWindow key win prevFocus moveRequest (keyEvt, keyState, _) = do
  rec
    focused <- memo $ (Just key==) <$> prevFocus
    focusedD <- minimizeChanges $ signalToDiscrete focused
    closeReq <- memoE $ closeRequest keyEvt focused
    translatedKeyEvt <- memoE $ translateKeyEvent metrics focused keyEvt
    reqMetrics <- memoD $ requestedMetrics key moveRequest metricsD
    let
      wInput = WindowInput
        { wiKey = translatedKeyEvt
        , wiKeyState = keyState
        , wiMetrics = metricsD
        , wiMetricsReq = reqMetrics
        , wiFocused = focusedD
        , wiCloseReq = closeReq
        }
    ((wDraw, initialMet, nextMet), val) <- win wInput
    metricsD <- delayD initialMet nextMet
    metrics <- discreteToSignal metricsD
    nextMetS <- discreteToSignal nextMet
  return ((,) <$> wDraw <*> nextMetS, val)

requestedMetrics
  :: (Eq k)
  => k
  -> Event (k, Vector2 Int)
  -> Discrete WindowMetrics
  -> Discrete WindowMetrics
requestedMetrics thisWindow moveRequest prevMetrics =
  applyMove <$> totalRequest <@> prevMetrics
  where
    applyMove vec (pos, size) = (position, size)
      where !position = vec2pos $ vec + pos2vec pos
    totalRequest = sum <$> eventToSignal reqsToThis
    reqsToThis = mapMaybeE toThis moveRequest
    toThis (destination, move)
      | destination == thisWindow = Just move
      | otherwise = Nothing

closeRequest :: Event KeyEvent -> Signal Bool -> Event ()
closeRequest keyEvt focused =
  filterNothingE $ mk <$> focused <@> keyEvt
  where
    mk True (MouseButton LeftButton, GL.Down, GL.Modifiers{ GL.alt = GL.Down }, _)
      = Just ()
    mk _ _ = Nothing

translateKeyEvent
  :: Signal WindowMetrics
  -> Signal Bool
  -> Event KeyEvent
  -> Event KeyEvent
translateKeyEvent metrics focused keyEvt =
  filterNothingE $ mk <$> metrics <*> focused <@> keyEvt
  where
    mk (windowPos, _) True (key, keyState, mods, cursorPos) =
      Just (key, keyState, mods, pos)
      where !pos = translatePosition windowPos cursorPos
    mk _ False _ = Nothing

translatePosition :: Position -> Position -> Position
translatePosition (Position windowX windowY) (Position x y) =
  Position (x - windowX) (y - windowY)

mapWindow :: (a -> b) -> Window a -> Window b
mapWindow f w = \input -> do
  (output, val) <- w input
  return (output, f val)
