module Main (main) where

import Language.Mecha
import Language.Mecha.Examples.CSG

main :: IO ()
main = do
  putStrLn "Writing csg.scad ..."
  writeFile "csg.scad" $ openSCAD csg
  putStrLn ""
  putStrLn "Open models with openscad, then compile and zoom in."
  putStrLn ""

