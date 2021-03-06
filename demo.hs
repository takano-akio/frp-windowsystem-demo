{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Main(main, test) where

import qualified DemoMain
import Util

import ElereaExtras.Event

import Control.Applicative
import Data.IORef
import FRP.Elerea.Simple
import Graphics.UI.GLUT
import System.Posix.Process (forkProcess)

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered]

  _ <- createWindow "demo"
  initialWindowSize $= Size 640 480
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 100 0 200 1
  diffuse (Light 0) $= Color4 0.1 0.4 0.1 1
  ambient (Light 0) $= Color4 0.1 0.4 0.1 1
  cullFace $= Just Back

  (update, draw) <- setupNetwork

  periodically 0 $ update >> draw

  mainLoop

setupNetwork :: IO (IO (), IO ())
setupNetwork = do
  (genKeyEvt, keyTrigger) <- externalEvent
  (mousePos, setMousePosition) <- external (Position 0 0)
  drawV <- newIORef $ return ()
  g <- start $ do
    size <- effectful (get windowSize)
    rawKeyEvt <- genKeyEvt
    keyEvt <- memoE $ apply (transKey <$> size) rawKeyEvt
    mousePosition <- memo $ transPos <$> size <*> mousePos
    DemoMain.main keyEvt mousePosition
  let
    update = writeIORef drawV . runTask =<< g
    draw = do
      clearColor $= Color4 0.2 0.2 0.2 0.0
      clear [ColorBuffer]

      id =<< readIORef drawV
      swapBuffers

  -- set callbacks
  keyboardMouseCallback $= Just (mkKeyCallback keyTrigger)
  reshapeCallback $= Just mkReshapeCallback
  motionCallback $= Just setMousePosition
  passiveMotionCallback $= Just setMousePosition

  return (update, draw)
  where
    mkKeyCallback keyTrigger key ks mods pos =
      keyTrigger (key, ks, mods, pos)
    mkReshapeCallback size@(Size w h) = do
      viewport $= (Position 0 0, size)
      loadIdentity
      ortho 0 (fromIntegral w) 0 (fromIntegral h) (-1000) 1000
    transKey (Size _ h) (key, ks, mods, (Position x y))
      = (key, ks, mods, Position x y')
      where !y' = fromIntegral h - y
    transPos (Size _ h) (Position x y) = Position x y'
      where !y' = fromIntegral h - y

periodically :: Int -> IO () -> IO ()
periodically ms act = addTimerCallback ms $ do
  act
  periodically ms act

-- for GHCi
test :: IO ()
test = forkProcess main >> return ()
