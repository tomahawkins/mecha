-- | Basic Example of Constructive Solid Geometry
module Main ( main ) where

import Control.Monad
import System.Environment

import Language.Mecha

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ putStrLn "usage: mecha-csg [ cadview | animate ]"
  when (elem "cadview" args) $ view "csg" 400 400 $ scaleAll 0.2 $ design
  when (elem "animate" args) $ animate "csg" 300 400 24 $ map f [0, 1 / 24 .. 5]
  where
  f t = (Perspective, moveY d3 $ rotateX (atan2 cz d2) $ rotateZ a design)
    where
    a = 1 * pi / 2 * t - pi / 2
    m = t
    cx = m * cos a
    cy = m * sin a
    cz = m
    d2 = sqrt $ cx ** 2 + cy ** 2
    d3 = sqrt $ cx ** 2 + cy ** 2 + cz ** 2

design :: Asm
design = assemble
  [ move ( 0, 0, 0) $ color (0.2, 0.2, 1, 0) $ part $ difference sphereCube cyl3
  , move (-4, 0, 0) $ color (0.8, 0, 0, 0) $ part $ sphereCube
  , move (-8, 0, 0) $ color (0.8, 0, 0, 0) $ part $ sphere'
  , move (-4, 4, 0) $ color (0.8, 0, 0, 0) $ part $ cube'
  , move ( 0, 4, 0) $ color (0, 0.8, 0, 0) $ part $ cyl3
  , move ( 0, 8, 0) $ color (0, 0.8, 0, 0) $ part $ cyl
  , move ( 0,12, 0) $ color (0, 0.8, 0, 0) $ part $ rotateX (pi/2) cyl
  , move ( 0,16, 0) $ color (0, 0.8, 0, 0) $ part $ rotateY (pi/2) cyl
  ]

sphere' = sphere 2
cube' = cube $ 2 * 0.75
sphereCube = intersection sphere' cube'
cyl = moveZ (-1) $ cylinder 1 2
cyl3 = unions [cyl, rotateX (pi / 2) cyl, rotateY (pi / 2) cyl]

