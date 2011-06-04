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
  = Primitive String String [Transform] Color
  | Union        Solid Solid
  | Intersection Solid Solid
  | Difference   Solid Solid
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
  Primitive    a b c d -> Primitive a b (c ++ [t]) d
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
    Primitive    a b t _ -> Primitive a b t c
    Union        a b     -> Union         (color c a) (color c b)
    Intersection a b     -> Intersection  (color c a) (color c b)
    Difference   a b     -> Difference    (color c a) (color c b)

instance Show Solid where
  show a = case a of
    Primitive a1 a2 t (r, g, b, o) -> printf "%s { %s\n%s%s}\n" a1 a2 trans color
      where
      color :: String
      color = printf "  pigment { rgbt <%f, %f, %f, %f> }\n" r g b o
      trans :: String
      trans = concatMap show t
    Union        a b   -> printf "merge        {\n%s%s}\n" (show a) (show b)
    Intersection a b   -> printf "intersection {\n%s%s}\n" (show a) (show b)
    Difference   a b   -> printf "difference   {\n%s%s}\n" (show a) (show b)

instance Show Transform where
  show a = case a of
    Scale (x, y, z) -> printf "  scale <%f, %f, %f>\n" x z y
    Move  (x, y, z) -> printf "  translate <%f, %f, %f>\n" x z y
    RotateX a       -> printf "  rotate <%f, 0, 0>\n" (-a * 180 / pi)
    RotateY a       -> printf "  rotate <0, 0, %f>\n" (-a * 180 / pi)
    RotateZ a       -> printf "  rotate <0, %f, 0>\n" (-a * 180 / pi)


primitive :: String -> String -> Solid
primitive a b = Primitive a b [] (0.5, 0.5, 0.5, 0)

-- | A sphere with diameter centered at origin.
sphere :: Double -> Solid
sphere d = primitive "sphere" $ printf "<0, 0, 0>, %f" (d / 2)

-- | A cube with edge length centered at origin.
cube :: Double -> Solid
cube a = box c c c
  where
  b = a / 2
  c = (-b, b)

-- | A cone with base at the origin, given base diameter, top diameter, and height.
cone :: Double -> Double -> Double -> Solid
cone bd td h = primitive "cone" $ printf "<0, 0, 0>, %f <0, %f, 0>, %f" (bd / 2) h (td / 2)

-- | A cylinder with base at the origin, given diameter and height.
cylinder :: Double -> Double -> Solid
cylinder d h = cone d d h

-- | A hollow cylinder with base at the origin, given outer diameter, inner diamter, and height.
tube od id h = difference (cylinder od h) (moveZ (-h) $ cylinder id (4 * h))

-- | A box with ranges or X, Y, and Z positions.
box :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Solid
box (x1, x2) (y1, y2) (z1, z2) = primitive "box" $ printf "<%f, %f, %f>, <%f, %f, %f>" x1 z1 y1 x2 z2 y2

-- | Arranges a solid in a radial pattern.
radial :: (Double -> Solid) -> Int -> Solid
radial f n = unions [ rotateZ a $ f a | i <- [0 .. n - 1], let a = 2 * pi * fromIntegral i / fromIntegral n ]

