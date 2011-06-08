module Main (main) where

import Language.Mecha
import Language.Mecha.Examples.CSG

main :: IO ()
main = do
  writeFile "csg.scad" $ openSCAD $ scaleAll 10 $ csg
  putStrLn ""
  putStrLn "Writing file: csg.scad"
  putStrLn ""
  putStrLn "Open with OpenSCAD, then click Design->Compile."
  putStrLn ""

