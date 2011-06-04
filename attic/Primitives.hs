module Language.Mecha.Primitives
  ( sphere
  , cube
  , cylinder
  ) where

import Language.Mecha.Solid

-- | A sphere with radius 1 centered at origin.
sphere :: Solid
sphere = Solid $ \ (x, y, z) -> sqrt (x ** 2 + y ** 2 + z ** 2) <= 1

-- | A sphere with edge length 2 centered at origin.
cube :: Solid
cube = Solid $ \ (x, y, z) -> all (\ a -> a <= 1 && a >= (-1)) [x, y, z]

-- | A cylinder with radius 1 and height 2 centered at origin.
cylinder :: Solid
cylinder = Solid $ \ (x, y, z) -> z <= 1 && z >= (-1) && sqrt (x ** 2 + y ** 2) <= 1

