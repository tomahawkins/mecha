-- | A timing plate with high and low pressure kidney ports.
module TimingPlate
  ( TimingPlate (..)
  , timingPlate
  ) where

import Language.Mecha

-- | Timing plate parameters.
data TimingPlate = TimingPlate
  { timingPlateT           :: Double  -- ^ Thickness.
  , timingPlateD           :: Double  -- ^ Total diameter.
  , timingPlateKidneyW     :: Double  -- ^ Kidney notch width.
  , timingPlateKidneyD     :: Double  -- ^ Kidney diameter, center to center.
  , timingPlateTransitionA :: Double  -- ^ Angle of high-pressure low-pressure transition.
  }

-- | Timing plate oriented on z-axis resting on x-y plane, with port kidneys left and right of the y-z plane.
timingPlate :: TimingPlate -> Asm
timingPlate t = part $ difference plate trim
  where
  plate = cylinder (timingPlateD t) (timingPlateT t)
  k = kidney ((timingPlateKidneyD t + timingPlateKidneyW t) / 2) ((timingPlateKidneyD t - timingPlateKidneyW t) / 2) (timingPlateTransitionA t)
  k' = scaleX (-1) k
  trim = scaleZ (2 * timingPlateT t) $ unions [k, k']

-- | Kidney cutter for timing plate.
--   kidney outerRadius innerRadius transitionAngle
kidney :: Double -> Double -> Double -> Solid
kidney or ir a = e
  where
  t = tube (or * 2) (ir * 2) 1
  b = box (0, - or * 3) (- or * 3, or * 3) (-2, 2)
  trim = union (rotateZ (-a) b) (rotateZ a b)
  round = moveX ((or + ir) / 2) $ cylinder (or - ir) 1
  d = unions [rotateZ (pi / 2 - a) round, rotateZ (a - pi / 2) round, difference t trim]
  e = moveZ (-1) $ scaleZ 2 d

