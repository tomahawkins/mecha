-- | Generate profiles and cross sections of solid models.
module Language.Mecha.Profile
  ( profile
  , crossSection
  , Point
  , Line
  ) where

import System.Directory
import System.Process

import Language.Mecha.Export
import Language.Mecha.Solid (Solid)

-- | Project profile to x-y plane and extract line data from OpenSCAD.
profile :: Solid -> IO [Line]
profile = cut False

-- | Capture cross section at x-y plane and extract line data form OpenSCAD.
crossSection :: Solid -> IO [Line]
crossSection = cut True

cut :: Bool -> Solid -> IO [Line]
cut cut a = do
  writeFile scad $ "projection(cut=" ++ (if cut then "true" else "false") ++ ")\n" ++ openSCAD a
  readProcess "OpenSCAD" ["-o", dxf, scad] ""
  f <- readFile dxf
  if length f < 0 then undefined else return ()
  removeFile scad
  removeFile dxf
  return $ parseDXF f
  where
  dxf  = "__mecha_input.dxf"
  scad = "__mecha_output.scad"

type Point = (Double, Double)
type Line  = (Point, Point)

-- | Extract the lines from an OpenSCAD DXF file.
parseDXF :: String -> [Line]
parseDXF = parse . lines
  where
  parse :: [String] -> [Line]
  parse a = case a of
    [] -> []
    "LINE" : _ : _ : _ : x0 : _ : x1 : _ : y0 : _ : y1 : rest -> ((read x0, read y0), (read x1, read y1)) : parse rest
    _ : rest -> parse rest

