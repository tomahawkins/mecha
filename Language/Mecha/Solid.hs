module Language.Mecha.Solid
  ( Solid
  , sphere
  , cone
  , box
  , cube
  , cylinder
  , tube
  , radial
  ) where

import Language.Mecha.Types
import Text.Printf

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

instance POVRay Solid where
  povray a = case a of
    Primitive t (r, g, b, o) a -> printf "%s { %s\n%s%s}\n" a1 a2 trans color
      where
      color :: String
      color = printf "  pigment { rgbt <%f, %f, %f, %f> }\n" r g b o
      trans :: String
      trans = concatMap povray t
      a1 :: String
      a2 :: String
      (a1, a2) = case a of
        Sphere d     -> ("sphere", printf "<0, 0, 0>, %f" (d / 2))
        Cone bd td h -> ("cone",   printf "<0, 0, 0>, %f <0, %f, 0>, %f" (bd / 2) h (td / 2))
        Box (x1, x2) (y1, y2) (z1, z2) -> ("box", printf "<%f, %f, %f>, <%f, %f, %f>" x1 z1 y1 x2 z2 y2)
    Union        a b   -> printf "merge        {\n%s%s}\n" (povray a) (povray b)
    Intersection a b   -> printf "intersection {\n%s%s}\n" (povray a) (povray b)
    Difference   a b   -> printf "difference   {\n%s%s}\n" (povray a) (povray b)

instance POVRay Transform where
  povray a = case a of
    Scale (x, y, z) -> printf "  scale <%f, %f, %f>\n" x z y
    Move  (x, y, z) -> printf "  translate <%f, %f, %f>\n" x z y
    RotateX a       -> printf "  rotate <%f, 0, 0>\n" (-a * 180 / pi)
    RotateY a       -> printf "  rotate <0, 0, %f>\n" (-a * 180 / pi)
    RotateZ a       -> printf "  rotate <0, %f, 0>\n" (-a * 180 / pi)

primitive :: Primitive -> Solid
primitive = Primitive [] (0.5, 0.5, 0.5, 0)

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

-- | A hollow cylinder with base at the origin, given outer diameter, inner diamter, and height.
tube od id h = difference (cylinder od h) (moveZ (-h) $ cylinder id (4 * h))

-- | A box with ranges or X, Y, and Z positions.
box :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Solid
box x y z = primitive $ Box x y z

-- | Arranges a solid in a radial pattern.
radial :: (Double -> Solid) -> Int -> Solid
radial f n = unions [ rotateZ a $ f a | i <- [0 .. n - 1], let a = 2 * pi * fromIntegral i / fromIntegral n ]

