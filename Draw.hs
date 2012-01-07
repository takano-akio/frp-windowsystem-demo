{-# OPTIONS_GHC -Wall #-}
-- | 2-D drawing combinators wrapping OpenGL functions.
module Draw
  ( Draw
  , square
  , cube
  , segment
  , stringD
  , multilineString
  , color
  , matcolor
  , matspec
  , shift
  , shiftZ
  , scale
  , scaleXY
  , rotate
  , rotateX
  , rotateY
  , runDraw
  , drawTask
  , textBoxSize
  , textBox
  , lighting
  , nolighting
  ) where

import Control.Applicative
import qualified Data.Sequence as Q
import Graphics.UI.GLUT hiding (shift, color, rotate, scale, lighting)
import qualified Graphics.UI.GLUT as GL
import qualified Data.Foldable as F
import Data.Monoid
import GLUtil
import Util

-- | Something to be drawn.
data Draw
  = PrimD (IO ())
  | TransD [Transformation] Draw
  | BracketD (IO () -> IO ()) Draw
  | MultiD (Q.Seq Draw)

type Transformation = IO ()

square :: Draw
square = PrimD $ renderPrimitive Quads $ mapM_ vertex
  [ Vertex3 1 1 (0::GLdouble)
  , Vertex3 (-1) 1 0
  , Vertex3 (-1) (-1) 0
  , Vertex3 1 (-1) 0
  ]

cube :: Draw
cube = pair `mappend` rotateX 90 pair `mappend` rotateY 90 pair
  where
    surface = defaultNormal $ shiftZ (-1) square
    pair = surface `mappend` rotateX 180 surface

defaultNormal :: Draw -> Draw
defaultNormal = trans (normal $ Normal3 0 0 (-1::GLdouble))

segment :: GLdouble -> Draw
segment sz = PrimD $ renderPrimitive GL.Lines $ mapM_ vertex
  [ Vertex3 (-sz) 0 0
  , Vertex3 sz 0 0
  ]

stringD :: String -> Draw
stringD = PrimD . renderString Roman

color :: Rgba -> Draw -> Draw
color col = trans (GL.color col)

matcolor :: Rgba -> Draw -> Draw
matcolor col = with (materialAmbientAndDiffuse Front) col

matspec :: Rgba -> Draw -> Draw
matspec col = with (materialSpecular Front) col

lighting :: Draw -> Draw
lighting = with GL.lighting GL.Enabled

nolighting :: Draw -> Draw
nolighting = with GL.lighting GL.Disabled

scale :: GLdouble -> Draw -> Draw
scale k = trans (GL.scale k k k)

scaleXY :: GLdouble -> GLdouble -> Draw -> Draw
scaleXY x y = trans (GL.scale x y 1)

shift :: GLdouble -> GLdouble -> Draw -> Draw
shift x y = trans (translate (Vector3 x y 0))

shiftZ :: GLdouble -> Draw -> Draw
shiftZ z = trans (translate (Vector3 0 0 z))

rotate :: GLdouble -> Draw -> Draw
rotate r = trans $ GL.rotate r (Vector3 0 0 1 :: Vector3 GLdouble)

rotateX :: GLdouble -> Draw -> Draw
rotateX r = trans $ GL.rotate r (Vector3 1 0 0 :: Vector3 GLdouble)

rotateY :: GLdouble -> Draw -> Draw
rotateY r = trans $ GL.rotate r (Vector3 0 1 0 :: Vector3 GLdouble)

trans :: IO () -> Draw -> Draw
trans tr (TransD ts body) = TransD (tr:ts) body
trans tr d = TransD [tr] d

with :: StateVar a -> a -> Draw -> Draw
with sv val = BracketD $ \body -> do
  old <- get sv
  sv $= val
  body
  sv $= old

instance Monoid Draw where
  mempty = MultiD Q.empty
  mappend x y = MultiD $ toSeq x Q.>< toSeq y

toSeq :: Draw -> Q.Seq Draw
toSeq (MultiD ds) = ds
toSeq d = Q.singleton d

runDraw :: Draw -> IO ()
runDraw = draw . nolighting . color (Color4 0.8 0.2 0.8 1)
  where
    draw (PrimD d) = d
    draw (TransD ts body) = unsafePreservingMatrix $ do
      sequence_ ts
      draw body
    draw (BracketD t body) = t $ draw body
    draw (MultiD ds) = F.traverse_ draw ds

drawTask :: Draw -> Task IO
drawTask = Task . runDraw

--------------------------------------------------------------------------------------
-- text formatting

multilineString :: String -> Draw
multilineString = mconcat . zipWith mk [0..] . reverse . lines
  where
    mk lineNum ln = shift 0 (lineNum*lineHeight + stringDescend) $ stringD ln

textBoxSize :: GLdouble -> String -> IO Size
textBoxSize textScale str = do
  width <- if null ls
    then return 0
    else fi . maximum <$> mapM (stringWidth Roman) ls
  return $! Size (mk width) (mk height)
  where
    ls = lines str
    height = fi (length ls) * lineHeight + stringDescend
    mk val = ceiling $ textScale * (val + 2 * textBoxMargin)

textBox :: String -> Draw
textBox = shift textBoxMargin textBoxMargin . multilineString

textBoxMargin :: GLdouble
textBoxMargin = 50

stringMaxHeight :: GLdouble
stringMaxHeight = 119.5

stringDescend :: GLdouble
stringDescend = 33.33

lineHeight :: GLdouble
lineHeight = (stringMaxHeight + stringDescend) * 1.05
