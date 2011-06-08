-- | A normal-vector piston pump.
module Language.Mecha.Examples.Pump (pump) where

import Language.Mecha

import Language.Mecha.Examples.Axle
import Language.Mecha.Examples.BarrelGroup
import Language.Mecha.Examples.Piston
import Language.Mecha.Examples.TimingPlate

pump :: Solid
pump = a -- unions [a, bg, p, tp]

a = color (0.8, 0.8, 0.8, 0.0) $ axle Axle
  { axleL    = 0.08
  , axleD    = 0.03
  , carrierT = 0.02
  , carrierD = 0.1
  }

tp = color (0.8, 0.8, 0.8, 0.0) $ timingPlate TimingPlate
  { timingPlateT           = 0.005
  , timingPlateD           = 0.1
  , timingPlateKidneyW     = 0.01
  , timingPlateKidneyD     = 0.08
  , timingPlateTransitionA = pi / 6
  }

bg = color (0.8, 0.8, 0.8, 0.0) $ barrelGroup BarrelGroup
  { barrelGroupH         = 0.08
  , barrelGroupD         = 0.1
  , barrelGroupN         = 7
  , barrelGroupBarrelD   = 0.01
  , barrelGroupOrbitalD  = 0.08
  }

p = color (0.8, 0.8, 0.8, 0.0) $ piston Piston
  { pistonD      = 0.02
  , pistonShaftD = 0.004
  , pistonL      = 0.08
  }

