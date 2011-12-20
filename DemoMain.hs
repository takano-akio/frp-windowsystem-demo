{-# LANGUAGE DoRec, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module DemoMain(main) where

import Draw
import GLUtil
import Util
import Window

import ElereaExts.Aggregate
import ElereaExts.Event
import ElereaExts.MonadSignalGen

import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import Data.UnixTime
import Graphics.UI.GLUT(GLdouble, Color4(..))
import qualified Graphics.UI.GLUT as GL
import System.Random
import Text.Printf

main :: Event KeyEvent -> Signal Position -> SignalGen (Signal (Task IO))
main keyEvt mousePos = runSignalGenA $ do
  curTime <- currentTime
  keyState <- keyStateFromKeyEvent keyEvt
  frameRate <- eventRate $ eachSample curTime

  let globalInput = (keyEvt, keyState, mousePos)
  let fps = makeFpsD <$> frameRate

  (windowDraw, sys) <- windowSystem globalInput
  rec
    windowColor <- colorToggleButton sys windowColor windowColorList
  _ <- simpleWindow sys (Size 200 100) $ simpleButton windowColor "foo"
  _ <- simpleWindow sys (Size 100 100) $ fpsWindow frameRate
  _ <- simpleWindow sys (Size 200 200) $ simpleButton windowColor "literally"

  return $! mconcat
    [ (drawTask <$> mconcat [fps, windowDraw])
    ]
  where
    windowColorList = [Color4 0.2 0.6 0.7 1, Color4 0.9 0.7 0.4 1]

colorToggleButton
  :: WindowSystem
  -> Signal (Color4 GLdouble)
  -> [Color4 GLdouble]
  -> SignalGenA (Signal (Color4 GLdouble))
colorToggleButton sys windowColor list = do
  click <- simpleWindow sys (Size 200 40) $
    simpleButton windowColor "Change color"
  colorSig <- accumB (cycle list) (const tail <$> click)
  return $ head <$> colorSig

fpsWindow :: Signal Double -> SimpleWindow
fpsWindow frameRate WindowInput{..} = do
  let fps = makeFpsD <$> frameRate
  background <- discreteToSignal $ bg <$> wiFocused <*> wiMetrics
  let draw = background `mappend` fps
  return (draw, mempty)
  where
    bg False = color (Color4 1 0.7 0.4 1) . windowBg
    bg True = color (Color4 1 0.8 0.3 1) . windowBg

clickEvent :: Event KeyEvent -> Event Position
clickEvent = filterNothingE . fmap pickupClick
  where
    pickupClick (MouseButton LeftButton, GL.Down, mods, pos)
      | mods == newtral = Just pos
    pickupClick _ = Nothing
    newtral = GL.Modifiers GL.Up GL.Up GL.Up

windowBg :: WindowMetrics -> Draw
windowBg (_, Size w h) =
  scaleXY (fromIntegral w) (fromIntegral h) $
  shift 0.5 0.5 $
  scale 0.5 $
  square

simpleButton :: Signal (Color4 GLdouble) -> String -> SimpleWindow
simpleButton unfocusedBgColor label WindowInput{..} = do
  d <- draw <$> discreteToSignal wiFocused <*> discreteToSignal wiMetrics
  click <- memoE $ const () <$> clickEvent wiKey
  return (d, click)
  where
    draw focused metrics = background `mappend` foreground
      where
        background = color <$> bgColor <*> (windowBg <$> metrics)
        foreground = pure $ scale 0.2 $ color black $ multilineString label
        bgColor = ifelse <$> focused <*> pure lighten <*> pure id <*> unfocusedBgColor
    lighten (Color4 r g b a) = Color4 (f r) (f g) (f b) a
      where f x = 1 - 0.7 * (1 - x)
    black = Color4 0 0 0 1

type SimpleWindow = WindowInput -> SignalGenA (Signal Draw, Event ())

-- | Creates a simple, random-positioned, user-closable and user-movable window
-- that returns a single unit event.
simpleWindow :: WindowSystem -> Size -> SimpleWindow -> SignalGenA (Event ())
simpleWindow sys size simple = do
  sig <- closableWindow sys w
  memoE $ joinEventSignal $ fromMaybe mempty <$> sig
  where
    w input = do
      pos <- execute $ randomWindowPos size
      let metrics = (pos, size)
      (draw, evt) <- simple input
      let output = (draw, metrics, wiMetricsReq input)
      return (output, (evt, wiCloseReq input))

randomWindowPos :: Size -> IO Position
randomWindowPos (Size w h) =
  Position <$> rnd (maxX - w) <*> rnd (maxY - h)
  where
    rnd n = fromIntegral <$> randomRIO (0::Int, fromIntegral n - 1)
    maxX = 640
    maxY = 480

--------------------------------------------------------------------------------
-- framerate calculation

makeFpsD :: Double -> Draw
makeFpsD frameRate = Draw.color (Color4 0.5 0.5 1 1) $
  Draw.scale 0.2 (stringD txt)
  where
    txt = printf "%.1ffps" frameRate

eventRate :: Event UnixTime -> SignalGenA (Signal Double)
eventRate time = do
  changes <- withPrev Nothing $ Just <$> time
  let diffs = fromMaybe 0 . (uncurry $ liftA2 diffUnixTime) <$> changes
  accumB 0 (updRate . realToFrac <$> diffs)
  where
    updRate diff current = rollingAverage (diff/0.1) current (1/max 1e-5 diff)

rollingAverage :: Double -> Double -> Double -> Double
rollingAverage timeSpan old new = p * old + (1 - p) * new
  where p = 0.5 ** timeSpan

currentTime :: SignalGenA (Signal UnixTime)
currentTime = effectful getUnixTime

--------------------------------------------------------------------------------

keyStateFromKeyEvent :: Event KeyEvent -> SignalGenA (Discrete KeyStateMap)
keyStateFromKeyEvent keyEvt = do
  m <- accumD M.empty (updMap <$> keyEvt)
  memoD $ flip (M.findWithDefault False) <$> m
  where
    updMap (key, ks, _, _) m = (M.insert key $! ks==GL.Down) m
