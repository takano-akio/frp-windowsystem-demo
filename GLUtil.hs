{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module GLUtil
  ( module GLUtil
  , Position(..)
  , Key(..)
  , GL.MouseButton(..)
  , GL.Size(..)
  , GL.Vector2(..)
  ) where

import Control.Applicative
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT(Key(..), Position(..), Vector2(..), GLdouble)

import Util

type KeyEvent = (Key, GL.KeyState, GL.Modifiers, Position)
type KeyStateMap = Key -> Bool
type Rgba = GL.Color4 GLdouble

------------------------------------------------------------------------------
-- 2-dimensional Vectors

-- | Component-wise arithmetic of 2d vectors
instance (Num a) => Num (Vector2 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = liftA negate
  abs = liftA abs
  signum = liftA signum
  fromInteger = toVec . fromInteger

instance (Fractional a) => Fractional (Vector2 a) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = toVec . fromRational

vectorLen :: (RealFloat a) => Vector2 a -> a
vectorLen (Vector2 x y) = sqrt (x*x + y*y)

toVec :: a -> Vector2 a
toVec x = Vector2 x x

pos2vec :: Position -> Vector2 Int
pos2vec (Position x y) = Vector2 (fi x) (fi y)

vec2pos :: Vector2 Int -> Position
vec2pos (Vector2 x y) = Position (fi x) (fi y)

normalizeVector :: (RealFloat a) => Vector2 a -> Vector2 a
normalizeVector vec
  | len == 0 = vec
  | otherwise = vec / toVec len
  where len = vectorLen vec
