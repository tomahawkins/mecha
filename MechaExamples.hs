module Main (main) where

import Language.Mecha
import Language.Mecha.Examples.CSG

main :: IO ()
main = do
  writeFile "csg.scad" $ openSCAD csg
  putStrLn ""
  putStrLn "Writing file: csg.scad"
  putStrLn ""
  putStrLn "Open with OpenSCAD, then compile and zoom in."
  putStrLn ""

