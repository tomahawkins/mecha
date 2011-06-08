module Main (main) where

import Language.Mecha
import Language.Mecha.Examples.CSG
import Language.Mecha.Examples.Pump

main :: IO ()
main = do
  putStrLn "Writing csg.scad ..."
  writeFile "csg.scad" $ openSCAD csg
  putStrLn "Writing pump.scad ..."
  writeFile "pump.scad" $ openSCAD pump
  putStrLn ""
  putStrLn "Open models with openscad, then compile and zoom in."
  putStrLn ""

