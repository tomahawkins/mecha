-- | Basic Example of Constructive Solid Geometry
module Main ( main ) where

import Language.Mecha

main :: IO ()
main = writeFile "csg.scad" $ openSCAD $ unions
  [ move ( 0, 0, 0) $ color (0.2, 0.2, 1.0, 1) $ difference sphereCube cyl3
  , move (-4, 0, 0) $ color (0.8, 0.0, 0.0, 1) $ sphereCube
  , move (-8, 0, 0) $ color (0.8, 0.0, 0.0, 1) $ sphere'
  , move (-4, 4, 0) $ color (0.8, 0.0, 0.0, 1) $ cube'
  , move ( 0, 4, 0) $ color (0.0, 0.8, 0.0, 1) $ cyl3
  , move ( 0, 8, 0) $ color (0.0, 0.8, 0.0, 1) $ cyl
  , move ( 0,12, 0) $ color (0.0, 0.8, 0.0, 1) $ rotateX (pi/2) cyl
  , move ( 0,16, 0) $ color (0.0, 0.8, 0.0, 1) $ rotateY (pi/2) cyl
  ]

sphere' = sphere 2
cube' = cube $ 2 * 0.75
sphereCube = intersection sphere' cube'
cyl = moveZ (-1) $ cylinder 1 2
cyl3 = unions [cyl, rotateX (pi / 2) cyl, rotateY (pi / 2) cyl]

