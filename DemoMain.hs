{-# LANGUAGE DoRec, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module DemoMain(main) where

import Draw
import GLUtil
import Util
import Window

import ElereaExtras.Aggregate
import ElereaExtras.Event
import ElereaExtras.MonadSignalGen

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import Data.UnixTime
import Graphics.UI.GLUT(Color4(..))
import qualified Graphics.UI.GLUT as GL
import System.Random
import Text.Printf

main :: Event KeyEvent -> Signal Position -> SignalGen (Signal (Task IO))
main keyEvt mousePos = runSignalGenA $ do
  keyState <- keyStateFromKeyEvent keyEvt
  let globalInput = (keyEvt, keyState, mousePos)

  (windowDraw, sys) <- windowSystem globalInput
  populateWindowSystem sys
  return $! drawTask <$> windowDraw

populateWindowSystem :: WindowSystem -> SignalGenA ()
populateWindowSystem sys = do
  curTime <- currentTime
  frameRate <- eventRate $ eachSample curTime
  rec
    windowColor <- colorToggleButton sys windowColor windowColorList
  execButton sys windowColor "Frame rate" $
    void $ simpleWindow sys (Size 100 100) $ fpsWindow frameRate
  execButton sys windowColor "Help" $
    noticeWindow sys windowColor helpString
  execButton sys windowColor "Animation" $ animationWindow sys
  where
    windowColorList = [Color4 0.2 0.6 0.7 1, Color4 0.9 0.7 0.4 1]

helpString :: String
helpString = unlines
  [ "Alt-click to close a window."
  , "Hold shift to drag a window."
  ]

execButton
  :: WindowSystem
  -> Signal Rgba
  -> String
  -> SignalGenA ()
  -> SignalGenA ()
execButton sys windowColor label action = do
  size <- execute $ simpleButtonSize label
  click <- simpleWindow sys size $ simpleButton windowColor label
  _ <- generatorE $ const action <$> click
  return ()

colorToggleButton
  :: WindowSystem
  -> Signal Rgba
  -> [Rgba]
  -> SignalGenA (Signal Rgba)
colorToggleButton sys windowColor list = do
  size <- execute $ simpleButtonSize text
  click <- simpleWindow sys size $
    simpleButton windowColor text
  colorSig <- accumB (cycle list) (const tail <$> click)
  return $ head <$> colorSig
  where text = "Change color"

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

simpleButton :: Signal Rgba -> String -> SimpleWindow
simpleButton unfocusedBgColor label WindowInput{..} = do
  d <- draw <$> discreteToSignal wiFocused <*> discreteToSignal wiMetrics
  click <- memoE $ const () <$> clickEvent wiKey
  return (d, click)
  where
    draw focused metrics = background `mappend` foreground
      where
        background = color <$> bgColor <*> (windowBg <$> metrics)
        foreground = pure $ scale 0.2 $ color black $ textBox label
        bgColor = ifelse <$> focused <*> pure lighten <*> pure id <*> unfocusedBgColor
    lighten (Color4 r g b a) = Color4 (f r) (f g) (f b) a
      where f x = 1 - 0.7 * (1 - x)
    black = Color4 0 0 0 1

simpleButtonSize :: String -> IO Size
simpleButtonSize label = textBoxSize 0.2 label

-- | An window that displays a static multiline text.
-- Closes itself when clicked.
noticeWindow :: WindowSystem -> Signal Rgba -> String -> SignalGenA ()
noticeWindow sys windowColor text = void $ closableWindow sys w
  where
    w input = do
      metrics <- execute $
        randomInitialMetrics =<< simpleButtonSize text
      (draw, clickEvt) <- simpleButton windowColor text input
      let output = (draw, metrics, wiMetricsReq input)
      return (output, ((), wiCloseReq input `mappend` clickEvt))

-- | Creates an window containing a simple animation demo.
animationWindow :: WindowSystem -> SignalGenA ()
animationWindow sys = void $ simpleWindow sys (Size 200 200) $ \input -> do
  objDraw <- animation
  metrics <- discreteToSignal $ wiMetrics input
  let bgDraw = bg <$> metrics
  return (bgDraw `mappend` objDraw, mempty)
  where
    animation = do
      r <- accumB 0 $ eachSample $ pure (+1)
      return $ shift 100 100 <$> (rotate <$> r <*> pure obj)
    obj = color col $ scale 20 square
    col = Color4 0.7 0.9 0.7 1
    bg met = color (Color4 0 0 0 1) $ windowBg met

type SimpleWindow = WindowInput -> SignalGenA (Signal Draw, Event ())

-- | Creates a simple, random-positioned, user-closable and user-movable window
-- that returns a single unit event.
simpleWindow :: WindowSystem -> Size -> SimpleWindow -> SignalGenA (Event ())
simpleWindow sys size simple = do
  sig <- closableWindow sys w
  memoE $ joinEventSignal $ fromMaybe mempty <$> sig
  where
    w input = do
      metrics <- execute $ randomInitialMetrics size
      (draw, evt) <- simple input
      let output = (draw, metrics, wiMetricsReq input)
      return (output, (evt, wiCloseReq input))

randomInitialMetrics :: Size -> IO WindowMetrics
randomInitialMetrics size = do
  pos <- randomWindowPos size
  return (pos, size)

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
