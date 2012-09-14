-- | Export model generation.
module Language.Mecha.Export
  ( povray
  , openSCAD
  ) where

import Text.Printf

import Language.Mecha.Solid

-- Generates a POV-Ray model.
povray :: Solid -> String
povray a = unlines
  [ "// Generated by Mecha (https://github.com/tomahawkins/mecha)"
  , ""
  , solid a
  , ""
  ]
  where

  solid :: Solid -> String
  solid a = case a of
    Primitive t (r, g, b, o) a -> printf "%s { %s\n%s%s}\n" a1 a2 (indent $ concatMap transform t) (indent color)
      where
      color :: String
      color = printf "pigment { rgbt <%f, %f, %f, %f> }\n" r g b (1 - o)
      a1 :: String
      a2 :: String
      (a1, a2) = case a of
        Sphere d     -> ("sphere", printf "<0, 0, 0>, %f" (d / 2))
        Cone bd td h -> ("cone",   printf "<0, 0, 0>, %f <0, %f, 0>, %f" (bd / 2) h (td / 2))
        Box (x1, x2) (y1, y2) (z1, z2) -> ("box", printf "<%f, %f, %f>, <%f, %f, %f>" xmin zmin ymin xmax zmax ymax)
          where
          xmin = min x1 x2
          xmax = max x1 x2
          ymin = min y1 y2
          ymax = max y1 y2
          zmin = min z1 z2
          zmax = max z1 z2
	Torus d1 d2  -> ("torus", printf "%f, %f" (d1 / 2) (d2 / 2))
    Union        a b   -> printf "merge        {\n%s%s}\n" (indent $ solid a) (indent $ solid b)
    Intersection a b   -> printf "intersection {\n%s%s}\n" (indent $ solid a) (indent $ solid b)
    Difference   a b   -> printf "difference   {\n%s%s}\n" (indent $ solid a) (indent $ solid b)

  transform :: Transform -> String
  transform a = case a of
    Scale (x, y, z) -> printf "scale <%f, %f, %f>\n" x z y
    Move  (x, y, z) -> printf "translate <%f, %f, %f>\n" x z y
    RotateX a       -> printf "rotate <%f, 0, 0>\n" (-a * 180 / pi)
    RotateY a       -> printf "rotate <0, 0, %f>\n" (-a * 180 / pi)
    RotateZ a       -> printf "rotate <0, %f, 0>\n" (-a * 180 / pi)

-- Generates an OpenSCAD model.
openSCAD :: Solid -> String
openSCAD a = unlines
  [ "// Generated by Mecha (https://github.com/tomahawkins/mecha)"
  , ""
  , solid a
  , ""
  ]
  where

  solid :: Solid -> String
  solid a = case a of
    Union        a b   -> printf "union()        {\n%s%s}\n" (indent $ solid a) (indent $ solid b)
    Intersection a b   -> printf "intersection() {\n%s%s}\n" (indent $ solid a) (indent $ solid b)
    Difference   a b   -> printf "difference()   {\n%s%s}\n" (indent $ solid a) (indent $ solid b)
    Primitive t (r, g, b, o) p -> printf "color([%f, %f, %f, %f]) %s\n" r g b o $ transform $ reverse t
      where
      transform :: [Transform] -> String
      transform a = case a of
        [] -> primitive p
        Scale (x, y, z) : rest -> printf "scale ([%f, %f, %f]) %s"     x y z          $ transform rest
        Move  (x, y, z) : rest -> printf "translate ([%f, %f, %f]) %s" x y z          $ transform rest
        RotateX a       : rest -> printf "rotate (%f, [1, 0, 0]) %s"   (a * 180 / pi) $ transform rest
        RotateY a       : rest -> printf "rotate (%f, [0, 1, 0]) %s"   (a * 180 / pi) $ transform rest
        RotateZ a       : rest -> printf "rotate (%f, [0, 0, 1]) %s"   (a * 180 / pi) $ transform rest

      primitive :: Primitive -> String
      primitive a = case a of
        Sphere d     -> printf "sphere(r = %f, $fn = 100);\n" (d / 2)
        Cone bd td h -> printf "cylinder(h = %f, r1 = %f, r2 = %f, center = false, $fn = 100);\n" h (bd / 2) (td / 2)
        Box (x1, x2) (y1, y2) (z1, z2) -> printf "translate ([%f, %f, %f]) cube(size = [%f, %f, %f], center = false);\n" xmin ymin zmin (xmax - xmin) (ymax - ymin) (zmax - zmin)
          where
          xmin = min x1 x2
          xmax = max x1 x2
          ymin = min y1 y2
          ymax = max y1 y2
          zmin = min z1 z2
          zmax = max z1 z2
        Torus d1 d2 -> printf "rotate_extrude($fn = 100) translate([%f, 0, 0]) circle(%f, $fn = 100);" (d1 / 2) (d2 / 2)

indent :: String -> String
indent a = unlines [ "\t" ++ l | l <- lines a ]

