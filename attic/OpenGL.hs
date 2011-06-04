module Language.Mecha.OpenGL
  ( vertex3
  , normal3
  , color3
  , scale3
  , translate3
  , rotate3
  ) where

import Graphics.Rendering.OpenGL

--vertex3 :: Real a => a -> a -> a -> IO ()
vertex3 x y z = vertex $ Vertex3 (toFloat x) (toFloat y) (toFloat z)

--normal3 :: Real a => a -> a -> a -> IO ()
normal3 x y z = normal $ Normal3 (toFloat x) (toFloat y) (toFloat z)

--color3 :: Real a => a -> a -> a -> IO ()
color3 r g b = color $ Color3 (toFloat r) (toFloat g) (toFloat b)

--scale3 :: Real a => a -> a -> a -> IO ()
scale3 x y z = scale (toFloat x) (toFloat y) (toFloat z)

--translate3 :: Real a => a -> a -> a -> IO ()
translate3 x y z = translate $ Vector3 (toFloat x) (toFloat y) (toFloat z)

--rotate3 :: (Real a, Floating a) => a -> a -> a -> a -> IO ()
rotate3 angle x y z = rotate (toFloat $ angle * 180 / pi) $ Vector3 (toFloat x) (toFloat y) (toFloat z)

toFloat :: (Real a, Floating a) => a -> GLfloat
toFloat = realToFrac
