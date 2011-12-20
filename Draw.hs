{-# OPTIONS_GHC -Wall #-}
-- | 2-D drawing combinators wrapping OpenGL functions.
module Draw
  ( Draw
  , square
  , segment
  , stringD
  , multilineString
  , color
  , shift
  , scale
  , scaleXY
  , rotate
  , runDraw
  , drawTask
  ) where

import qualified Data.Sequence as Q
import Graphics.UI.GLUT hiding (shift, color, rotate, scale)
import qualified Graphics.UI.GLUT as GL
import qualified Data.Foldable as F
import Data.Monoid
import Util

-- | Something to be drawn.
data Draw
  = PrimD (IO ())
  | TransD [Transformation] Draw
  | MultiD (Q.Seq Draw)

type Transformation = IO ()

square :: Draw
square = PrimD $ renderPrimitive Quads $ mapM_ vertex
  [ Vertex3 1 1 (0::GLdouble)
  , Vertex3 (-1) 1 0
  , Vertex3 (-1) (-1) 0
  , Vertex3 1 (-1) 0
  ]

segment :: GLdouble -> Draw
segment sz = PrimD $ renderPrimitive GL.Lines $ mapM_ vertex
  [ Vertex3 (-sz) 0 0
  , Vertex3 sz 0 0
  ]

stringD :: String -> Draw
stringD = PrimD . renderString Roman

multilineString :: String -> Draw
multilineString = mconcat . zipWith mk [0..] . reverse . lines
  where
    mk lineNum ln = shift 0 (lineNum*lineHeight + stringDescend) $ stringD ln
    lineHeight = (stringMaxHeight + stringDescend) * 1.05

stringMaxHeight :: GLdouble
stringMaxHeight = 119.5

stringDescend :: GLdouble
stringDescend = 33.33

color :: Color4 GLdouble -> Draw -> Draw
color col = trans (GL.color col)

scale :: GLdouble -> Draw -> Draw
scale k = trans (GL.scale k k k)

scaleXY :: GLdouble -> GLdouble -> Draw -> Draw
scaleXY x y = trans (GL.scale x y 1)

shift :: GLdouble -> GLdouble -> Draw -> Draw
shift x y = trans (translate (Vector3 x y 0))

rotate :: GLdouble -> Draw -> Draw
rotate r = trans $ GL.rotate r (Vector3 0 0 1 :: Vector3 GLdouble)

trans :: IO () -> Draw -> Draw
trans tr (TransD ts body) = TransD (tr:ts) body
trans tr d = TransD [tr] d

instance Monoid Draw where
  mempty = MultiD Q.empty
  mappend x y = MultiD $ toSeq x Q.>< toSeq y

toSeq :: Draw -> Q.Seq Draw
toSeq (MultiD ds) = ds
toSeq d = Q.singleton d

runDraw :: Draw -> IO ()
runDraw = draw . color (Color4 0.8 0.2 0.8 1)
  where
    draw (PrimD d) = d
    draw (TransD ts body) = unsafePreservingMatrix $ do
      sequence_ ts
      draw body
    draw (MultiD ds) = F.traverse_ draw ds

drawTask :: Draw -> Task IO
drawTask = Task . runDraw
