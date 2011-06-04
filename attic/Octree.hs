module Language.Mecha.Octree
  ( Octree (..)
  , mesh
  ) where

import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Graphics.Rendering.OpenGL as GL

import Language.Mecha.OpenGL
import Language.Mecha.Types

data Octree
  = Octree  { center :: Vertex, radius :: Double, u1, u2, u3, u4, l1, l2, l3, l4 :: Octree }
  | Surface { point  :: Vertex, normal :: Vertex, shade :: Color }
  | Inside
  | Outside deriving (Show, Eq)

instance Setable Octree where
  union (Octree c r a0 a1 a2 a3 a4 a5 a6 a7) (Octree _ _ b0 b1 b2 b3 b4 b5 b6 b7) = if allInside' then Inside else x
    where
    x0 = union a0 b0
    x1 = union a1 b1
    x2 = union a2 b2
    x3 = union a3 b3
    x4 = union a4 b4
    x5 = union a5 b5
    x6 = union a6 b6
    x7 = union a7 b7
    x  = Octree c r x0 x1 x2 x3 x4 x5 x6 x7
    allInside' = allInside [x0, x1, x2, x3, x4, x5, x6, x7]
  union Inside _  = Inside
  union _ Inside  = Inside
  union Outside a = a
  union a Outside = a
  union a _       = a
  
  intersection (Octree c r a0 a1 a2 a3 a4 a5 a6 a7) (Octree _ _ b0 b1 b2 b3 b4 b5 b6 b7) = if allOutside' then Outside else x
    where
    x0 = intersection a0 b0
    x1 = intersection a1 b1
    x2 = intersection a2 b2
    x3 = intersection a3 b3
    x4 = intersection a4 b4
    x5 = intersection a5 b5
    x6 = intersection a6 b6
    x7 = intersection a7 b7
    x  = Octree c r x0 x1 x2 x3 x4 x5 x6 x7
    allOutside' = allOutside [x0, x1, x2, x3, x4, x5, x6, x7]
  intersection Inside a  = a
  intersection a Inside  = a
  intersection Outside _ = Outside
  intersection _ Outside = Outside
  intersection a _       = a
  
  difference (Octree c r a0 a1 a2 a3 a4 a5 a6 a7) (Octree _ _ b0 b1 b2 b3 b4 b5 b6 b7) = if allOutside' then Outside else x
    where
    x0 = difference a0 b0
    x1 = difference a1 b1
    x2 = difference a2 b2
    x3 = difference a3 b3
    x4 = difference a4 b4
    x5 = difference a5 b5
    x6 = difference a6 b6
    x7 = difference a7 b7
    x  = Octree c r x0 x1 x2 x3 x4 x5 x6 x7
    allOutside' = allOutside [x0, x1, x2, x3, x4, x5, x6, x7]
  difference _ Inside  = Outside
  difference a Outside = a
  difference Outside _ = Outside
  difference _ (Octree c r b0 b1 b2 b3 b4 b5 b6 b7) = Octree c r (d b0) (d b1) (d b2) (d b3) (d b4) (d b5) (d b6) (d b7)
    where
    d = difference Inside
  difference _ (Surface p (x, y, z) c) = Surface p (-x, -y, -z) c

type Path = [Octant]
type Octant = (Bool, Bool, Bool)
data Axis = X | Y | Z deriving Eq
type Direction = (Axis, Bool)
type Context = [(Octree, Octant)]

neighbor :: Context -> Direction -> (Context, Octree)
neighbor context (axis, sign) = neighbor context []
  where
  neighbor :: Context -> Path -> (Context, Octree)
  neighbor [] _ = ([], Outside)
  neighbor ((octree, (x, y, z)) : context) path = case axis of
    X | xor sign x -> subOctree context xPath octree
      | otherwise  -> neighbor  context xPath
    Y | xor sign y -> subOctree context yPath octree
      | otherwise  -> neighbor  context yPath
    Z | xor sign z -> subOctree context zPath octree
      | otherwise  -> neighbor  context zPath
    where
    xPath = (not x, y, z) : path
    yPath = (x, not y, z) : path
    zPath = (x, y, not z) : path

octant :: Octant -> Octree -> Octree
octant (x, y, z) = if y then a else b
  where
  (uA, uB, lA, lB) = if x then (u1, u4, l1, l4) else (u2, u3, l2, l3)
  (a, b) = if z then (uA, uB) else (lA, lB)

subOctree :: Context -> Path -> Octree -> (Context, Octree)
subOctree context []    octree = (context, octree)
subOctree context (a:b) octree = case octree of
  Octree _ _ _ _ _ _ _ _ _ _ -> subOctree ((octree, a) : context) b (octant a octree)
  _ -> (context, octree)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

allInside :: [Octree] -> Bool
allInside a = all (== Inside) a

allOutside :: [Octree] -> Bool
allOutside a = all (== Outside) a

allSurface :: [Octree] -> Bool
allSurface = all isSurface

isSurface :: Octree -> Bool
isSurface (Surface _ _ _) = True
isSurface _               = False

mesh :: Octree -> IO (IO ())
mesh octree = do
  return $ do
    GL.renderPrimitive GL.Quads $ render True  quads
    GL.lighting GL.$= GL.Disabled
    GL.depthMask GL.$= GL.Disabled
    --GL.lineWidth GL.$= 2
    color3 0 0 0
    GL.renderPrimitive GL.Lines $ render False lines
    GL.depthMask GL.$= GL.Enabled
    GL.lighting GL.$= GL.Enabled
  where
  mesh = meshVertices [] octree
  quads :: [Int]
  quads = concat [ [id a, id b, id c, id d] | (_, (_, _, a, _)) <- mesh, (a, b, c, d) <- a ]
  lines = concat [ [id a, id b, id b, id c, id c, id d, id d, id a] | (_, (_, _, a, _)) <- mesh, (a@(x, y, z), b, c, d) <- a, x >= 0, y >= 0, z >= 0 ]
  colors  = IM.fromList [ (ids M.! a, color)  | (a, (_, _, _, color))  <- mesh ]
  normals = IM.fromList [ (ids M.! a, normal) | (a, (normal, _, _, _)) <- mesh ]
  vertices1 = fst $ unzip mesh
  ids = M.fromList $ zip vertices1 [0..]
  id a = ids M.! a
  vertices = IM.fromList $ zip [0..] vertices1
  render poly a = sequence_ $ map glCmd $ glCmdOpt $ concat $ map (glCmds poly) a

  glCmds :: Bool -> Int -> [GlCmd]
  glCmds poly i = (if poly then [C c1 c2 c3, N n1 n2 n3] else []) ++ [V v1 v2 v3]
    where
    (c1, c2, c3) = colors   IM.! i
    (n1, n2, n3) = normals  IM.! i
    (v1, v2, v3) = vertices IM.! i

glCmd :: GlCmd -> IO ()
glCmd a = case a of
  C a b c -> color3  a b c
  N a b c -> normal3 a b c
  V a b c -> vertex3 a b c

glCmdOpt :: [GlCmd] -> [GlCmd]
glCmdOpt [] = []
glCmdOpt (a:b) = a : f a b
  where
  f _ [] = []
  f lastColor (a:b) = case a of
    C _ _ _ | a == lastColor -> f lastColor b
            | otherwise      -> a : f a b
    _                        -> a : f lastColor b

data GlCmd
  = C Double Double Double
  | N Double Double Double
  | V Double Double Double
  deriving Eq

meshVertices :: Context -> Octree -> [(Vertex, (Vertex, [Vertex], [(Vertex, Vertex, Vertex, Vertex)], Color))]
meshVertices context octree = case octree of
  Inside  -> []
  Outside -> []
  Surface p normal color -> [(p, (normal, a ++ b ++ c, m ++ n ++ o, color))]
    where
    a = if isSurface xp then [point xp] else []
    b = if isSurface yp then [point yp] else []
    c = if isSurface zp then [point zp] else []
    m = f xp xpyp yp
    n = f yp ypzp zp
    o = f xp xpzp zp
    (xpC, xp) = neighbor context (X, True)
    (ypC, yp) = neighbor context (Y, True)
    (_,   zp) = neighbor context (Z, True)
    (_, xpyp) = neighbor xpC (Y, True)
    (_, ypzp) = neighbor ypC (Z, True)
    (_, xpzp) = neighbor xpC (Z, True)
    f :: Octree -> Octree -> Octree -> [(Vertex, Vertex, Vertex, Vertex)]
    f a b c = case (isSurface a, isSurface b, isSurface c) of
      (True,  True,  True)  -> [(p, point a, point b, point c)]
      (True,  False, True)  -> [(p, point a, point a, point c)]  -- XXX Redundent.
      (True,  True,  False) -> [(p, point a, point a, point b)]  -- XXX Redundent.
      (False, True,  True)  -> [(p, point b, point b, point c)]  -- XXX Redundent.
      _                     -> []
  octree -> concat
    [ meshVertices ((octree, (True,  True,  True )) : context) $ u1 octree
    , meshVertices ((octree, (False, True,  True )) : context) $ u2 octree
    , meshVertices ((octree, (False, False, True )) : context) $ u3 octree
    , meshVertices ((octree, (True,  False, True )) : context) $ u4 octree
    , meshVertices ((octree, (True,  True,  False)) : context) $ l1 octree
    , meshVertices ((octree, (False, True,  False)) : context) $ l2 octree
    , meshVertices ((octree, (False, False, False)) : context) $ l3 octree
    , meshVertices ((octree, (True,  False, False)) : context) $ l4 octree
    ]

