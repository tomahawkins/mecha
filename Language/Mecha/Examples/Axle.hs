-- | Axle and ball joint carrier.
module Language.Mecha.Examples.Axle
  ( Axle (..)
  , axle
  ) where

import Language.Mecha

-- | Axle parameters.
data Axle = Axle
  { axleL        :: Double  -- ^ Axle length.
  , axleD        :: Double  -- ^ Axle diameter.
  , carrierT     :: Double  -- ^ Carrier thickness.
  , carrierD     :: Double  -- ^ Carrier diameter.
  }

-- | Axle oriented on z-axis resting on x-y plane.
axle :: Axle -> Solid
axle a = union shaft carrier
  where
  carrier = cylinder (carrierD a) (carrierT a)
  shaft   = difference (cylinder (axleD a) (axleL a + carrierT a)) spines
  spines  = radial (const spine) 10
  spine   = moveX (axleD a / 2) $ cylinder (axleD a / 6) ((axleL a + carrierT a) * 2)

