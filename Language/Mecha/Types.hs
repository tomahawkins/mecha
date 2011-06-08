module Language.Mecha.Types
  ( Vector, Vertex, Normal, Color
  , Moveable  (..)
  , Scaleable (..)
  , Colorable (..)
  , Setable   (..)
  , moveX
  , moveY
  , moveZ
  , scaleAll
  , scaleX
  , scaleY
  , scaleZ
  , unions
  ) where

type Vector = (Double, Double, Double)
type Vertex = Vector
type Normal = Vector
type Color  = (Double, Double, Double, Double)

class Moveable a where
  move    :: Vector -> a -> a
  rotateX :: Double -> a -> a
  rotateY :: Double -> a -> a
  rotateZ :: Double -> a -> a

moveX :: Moveable a => Double -> a -> a
moveX a = move (a, 0, 0)

moveY :: Moveable a => Double -> a -> a
moveY a = move (0, a, 0)

moveZ :: Moveable a => Double -> a -> a
moveZ a = move (0, 0, a)

class Scaleable a where
  scale :: Vector -> a -> a

scaleAll :: Scaleable a => Double -> a -> a
scaleAll a = scale (a, a, a)

scaleX :: Scaleable a => Double -> a -> a
scaleX a = scale (a, 1, 1)

scaleY :: Scaleable a => Double -> a -> a
scaleY a = scale (1, a, 1)

scaleZ :: Scaleable a => Double -> a -> a
scaleZ a = scale (1, 1, a)

class Colorable a where
  color :: Color -> a -> a

class Setable a where
  union        :: a -> a -> a
  intersection :: a -> a -> a
  difference   :: a -> a -> a

unions :: Setable a => [a] -> a
unions = foldl1 union

