{-# OPTIONS_GHC -Wall #-}
module GLUtil
  ( module GLUtil
  , Position(..)
  , Key(..)
  , GL.MouseButton(..)
  , GL.Size(..)
  ) where

import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT(Key(..), Position(..))

type KeyEvent = (Key, GL.KeyState, GL.Modifiers, Position)
type KeyStateMap = Key -> Bool
