-- | A rotational barrel kit.
module Language.Mecha.Examples.BarrelGroup
  ( BarrelGroup (..)
  , barrelGroup
  ) where

import Language.Mecha

-- | Barrel group parameters.
data BarrelGroup = BarrelGroup
  { barrelGroupH           :: Double  -- ^ Height.
  , barrelGroupD           :: Double  -- ^ Diameter.
  , barrelGroupN           :: Int     -- ^ Number of barrels (pistons).
  , barrelGroupBarrelD     :: Double  -- ^ Barrel diameter.
  , barrelGroupOrbitalD    :: Double  -- ^ Piston orbital diameter.
  }

-- | Barrel group oriented on z-axis resting on x-y plane, with barrel centered on positive x-axis.
barrelGroup :: BarrelGroup -> Solid
barrelGroup b = difference a $ radial (const c) (barrelGroupN b)
  where
  a = cylinder (barrelGroupD b) (barrelGroupH b)
  c = moveX (barrelGroupOrbitalD b / 2) $ moveZ (- barrelGroupH b) $ cylinder (barrelGroupBarrelD b) (barrelGroupH b * 3)

