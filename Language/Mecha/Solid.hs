module Language.Mecha.Solid
  ( Solid     (..)
  , Primitive (..)
  , Transform (..)
  , sphere
  , cone
  , box
  , cube
  , cylinder
  , cylinder'
  , tube
  , radial
  , torus
  ) where

import Language.Mecha.Types

data Solid
  = Primitive [Transform] Color Primitive
  | Union        Solid Solid
  | Intersection Solid Solid
  | Difference   Solid Solid
  deriving Eq

data Primitive
  = Sphere Double                -- ^ Diameter.
  | Cone   Double Double Double  -- ^ Bottom diameter, top diameter, height.
  | Box (Double, Double) (Double, Double) (Double, Double)  -- ^ (x min, x max) (y min, ymax) (z min, z max).
  | Torus  Double Double         -- ^ Major diameter, minor diameter.
  deriving Eq

data Transform
  = Scale (Vector)
  | Move  (Vector)
  | RotateX Double
  | RotateY Double
  | RotateZ Double
  deriving Eq

transform :: Transform -> Solid -> Solid
transform t a = case a of
  Primitive    a b c   -> Primitive (a ++ [t]) b c
  Union        a b     -> Union         (transform t a) (transform t b)
  Intersection a b     -> Intersection  (transform t a) (transform t b)
  Difference   a b     -> Difference    (transform t a) (transform t b)

instance Moveable Solid where
  move a    = transform $ Move a
  rotateX a = transform $ RotateX a
  rotateY a = transform $ RotateY a
  rotateZ a = transform $ RotateZ a

instance Scaleable Solid where
  scale a   = transform $ Scale a

instance Setable Solid where
  union        = Union
  intersection = Intersection
  difference   = Difference

instance Colorable Solid where
  color c a = case a of
    Primitive    a _ b   -> Primitive a c b
    Union        a b     -> Union         (color c a) (color c b)
    Intersection a b     -> Intersection  (color c a) (color c b)
    Difference   a b     -> Difference    (color c a) (color c b)

primitive :: Primitive -> Solid
primitive = Primitive [] (0.5, 0.5, 0.5, 1)

-- | A sphere with diameter, centered at origin.
sphere :: Double -> Solid
sphere = primitive . Sphere

-- | A cube with edge length, centered at origin.
cube :: Double -> Solid
cube a = box c c c
  where
  b = a / 2
  c = (-b, b)

-- | A cone with base at the origin, given base diameter, top diameter, and height.
cone :: Double -> Double -> Double -> Solid
cone bd td h = primitive $ Cone bd td h

-- | A cylinder with base at the origin, given diameter and height.
cylinder :: Double -> Double -> Solid
cylinder d h = cone d d h

-- | Same as cylinder, but centered at the origin.
cylinder' :: Double -> Double -> Solid
cylinder' d h = moveZ (- h / 2) $ cylinder d h

-- | A hollow cylinder with base at the origin, given outer diameter, inner diamter, and height.
tube :: Double -> Double -> Double -> Solid
tube od id h = difference (cylinder od h) (moveZ (-h) $ cylinder id (4 * h))

-- | A box with ranges or X, Y, and Z positions.
box :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Solid
box x y z = primitive $ Box x y z

-- | Arranges a solid in a radial pattern.
radial :: (Double -> Solid) -> Int -> Solid
radial f n = unions [ rotateZ a $ f a | i <- [0 .. n - 1], let a = 2 * pi * fromIntegral i / fromIntegral n ]

-- | A torus centered at the origin, aligned on the z-axis, with the major and minor diameters.
torus :: Double -> Double -> Solid
torus d1 d2 = primitive $ Torus d1 d2

