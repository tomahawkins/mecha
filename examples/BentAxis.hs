-- | A simple model of a bent-axis piston pump.
module Main ( main ) where

import Control.Monad
import Language.Mecha
import System.Environment

import Axle
import BarrelGroup
import Piston
import TimingPlate

main :: IO ()
main = do
  args <- getArgs
  let on arg action = when (elem arg args) action
  when (null args) $ putStrLn "usage: mecha-bent-axis [ axle | barrel-group | piston | timing-plate ]"
  on "axle"         $ view "axle"         400 400 $ scaleAll 4.0 $ a
  on "barrel-group" $ view "barrel-group" 400 400 $ scaleAll 4.0 $ bg
  on "piston"       $ view "piston"       400 400 $ scaleAll 4.0 $ p
  on "timing-plate" $ view "timing-plate" 400 400 $ scaleAll 4.0 $ tp
  --on "animate" args) $ animate "example" 300 400 24 $ map f [0, 1 / 24 .. 5]

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

