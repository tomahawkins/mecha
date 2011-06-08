-- | Piston.
module Language.Mecha.Examples.Piston
  ( Piston (..)
  , piston
  ) where

import Language.Mecha

-- | Piston parameters.
data Piston = Piston
  { pistonD      :: Double  -- ^ Diameter of both piston head and ball joint.
  , pistonShaftD :: Double  -- ^ Diameter shaft between piston head and ball joint.
  , pistonL      :: Double  -- ^ Length from ball center to ball center.
  }

-- | Piston oriented on z-axis with ball joint centered at origin.
piston :: Piston -> Solid
piston p = unions [ball, moveZ (pistonL p) ball, shaft]
  where
  ball = sphere $ pistonD p
  shaft = cylinder (pistonShaftD p) (pistonL p)

