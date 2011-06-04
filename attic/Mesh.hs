module Language.Mecha.Mesh
  ( mesh
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import Language.Mecha.Solid
import Language.Mecha.Types hiding (rotateX, rotateY, rotateZ)

-- | Creates a triangle mesh from a solid.
mesh :: Double -> Double -> Int -> Solid -> [(Vector, Vector)]  -- [(normal, vector), ...]
mesh radius p n solid = polygons
  where
  num = ceiling $ radius / p
  i = [-num .. num]
  j = [-num .. num - 1]
  vertices :: Map (Int, Int, Int) (Maybe Vertex, Maybe Vertex, Maybe Vertex)
  vertices = M.fromList [ ((x, y, z), (f (x + 1, y, z), f (x, y + 1, z), f (x, y, z + 1))) | x <- i, y <- i, z <- i, let f = edge (x,y,z) ]
  edge (ax, ay, az) (bx, by, bz) = sdEdge solid n (p * fromIntegral ax, p * fromIntegral ay, p * fromIntegral az) (p * fromIntegral bx, p * fromIntegral by, p * fromIntegral bz)
  polygons = concat [ cubePolygons (x, y, z) a | x <- j, y <- j, z <- j, let a = corners solid p (x, y, z), or a, not (and a) ]

  cubePolygons :: (Int, Int, Int) -> [Bool] -> [(Vector, Vector)]
  cubePolygons cube config = normals $ map (f . vertexIndex cube) $ polygonConfigurations M.! config
    where
    f :: ((Int, Int, Int), Axis) -> Vertex
    f ((x, y, z), a) = fromJust $ case a of
      X -> x'
      Y -> y'
      Z -> z'
      where
      (x', y', z') = vertices M.! (x, y, z)

sdEdge :: Solid -> Int -> Vertex -> Vertex -> Maybe Vertex
sdEdge (Solid f) n a b
  | f a && f b || not (f a) && not (f b) = Nothing
  | otherwise = Just $ sd n a b
  where
  sd :: Int -> Vertex -> Vertex -> Vertex
  sd n a b | n <= 0     = m
           | f a == f m = sd (n - 1) m b
           | otherwise  = sd (n - 1) a m
    where
    m = average a b

average :: Vector -> Vector -> Vector
average (aX, aY, aZ) (bX, bY, bZ) = ((aX+bX)/2, (aY+bY)/2, (aZ+bZ)/2)

corners :: Solid -> Double -> (Int, Int, Int) -> [Bool]
corners (Solid f) p (x, y, z) = map m
  [ (x,     y,     z)
  , (x + 1, y,     z)
  , (x + 1, y + 1, z)
  , (x,     y + 1, z)
  , (x,     y,     z + 1)
  , (x + 1, y,     z + 1)
  , (x + 1, y + 1, z + 1)
  , (x,     y + 1, z + 1)
  ]
  where
  m (x, y, z) = f (p * fromIntegral x, p * fromIntegral y, p * fromIntegral z)

normals :: [Vector] -> [(Vector, Vector)]  -- Normals follow right hand rule for triangles.
normals [] = []
normals (a:b:c:d) = [(normal, a), (normal, b), (normal, c)] ++ normals d
  where
  (ax, ay, az) = a
  (bx, by, bz) = b
  (cx, cy, cz) = c
  vx = bx - ax
  vy = by - ay
  vz = bz - az
  wx = cx - ax
  wy = cy - ay
  wz = cz - az
  mx = vy * wz - vz * wy
  my = vz * wx - vx * wz
  mz = vx * wy - vy * wx
  mag = sqrt $ mx ** 2 + my ** 2 + mz ** 2
  normal = (mx / mag, my / mag, mz / mag)
normals _ = undefined

patterns :: [([Bool], [Edge])]
patterns =
  [ ([x, o, o, o, o, o, o, o], [A, D, E])

  , ([x, x, o, o, o, o, o, o], [B, D, F, F, D, E])
  , ([x, o, o, o, o, x, o, o], [A, D, E, I, J, F])
  , ([x, o, o, o, o, o, x, o], [A, D, E, J, K, G])

  , ([o, x, x, x, o, o, o, o], [F, G, H, H, D, F, D, A, F])
  , ([x, x, o, o, o, o, x, o], [F, B, D, F, D, E, J, K, G])
  , ([o, x, o, o, x, o, x, o], [F, B, A, J, K, G, E, L, I])

  , ([x, x, x, x, o, o, o, o], [E, F, G, G, H, E])
  , ([o, x, x, x, x, o, o, o], [F, G, H, F, H, D, A, F, D, E, L, I])
  , ([x, o, x, o, o, x, o, x], [A, D, E, B, G, C, F, I, J, K, L, H])
  , ([x, o, x, x, o, o, o, x], [B, G, A, A, G, K, A, K, E, E, K, L])
  , ([o, x, x, x, o, o, o, x], [G, K, L, G, L, A, A, L, D])
  , ([x, o, x, o, x, o, x, o], [B, J, C, K, C, J, D, I, A, D, L, I])
  , ([x, o, x, x, o, o, x, o], [B, J, A, A, J, H, H, J, K, A, H, E])
  ]
  where
  x = True
  o = False

mirrorX [a,b,c,d,e,f,g,h] = [b,a,d,c,f,e,h,g]
mirrorX _ = undefined
mirrorY [a,b,c,d,e,f,g,h] = [d,c,b,a,h,g,f,e]
mirrorY _ = undefined
rotateX [a,b,c,d,e,f,g,h] = [e,f,b,a,h,g,c,d]
rotateX _ = undefined
rotateY [a,b,c,d,e,f,g,h] = [b,f,g,c,a,e,h,d]
rotateY _ = undefined
rotateZ [a,b,c,d,e,f,g,h] = [d,a,b,c,h,e,f,g]
rotateZ _ = undefined
rotateXZ = rotateZ . rotateX

data Op = Invert | RotateXZ | RotateX | RotateY | MirrorX | MirrorY deriving Show
data Axis = X | Y | Z

polygonConfigurations :: Map [Bool] [Edge]
polygonConfigurations = M.fromList [ (a, f a) | a <- allConfigs ]
  where
  allConfigs = filter (\ a -> or a && not (and a)) $ sequence (replicate 8 a) where a = [True, False]
  f :: [Bool] -> [Edge]
  f config = foldr unOp edges ops
    where
    (_, ops, edges) = findPattern config

findPattern :: [Bool] -> ([Bool], [Op], [Edge])
findPattern a = head [ (config, ops, fromJust $ lookup config patterns) | (config, ops) <- orient [Invert, RotateXZ, RotateXZ, RotateX, RotateY, MirrorX, MirrorY] [] a, elem config $ fst $ unzip $ patterns ]
  where
  orient :: [Op] -> [Op] -> [Bool] -> [([Bool], [Op])]
  orient [] ops a = [(a, reverse ops)]
  orient (f:fs) ops a = orient fs ops a ++ orient fs (f:ops) (op f a)
  op f = case f of
    Invert   -> map not
    RotateXZ -> rotateXZ
    RotateX  -> rotateX
    RotateY  -> rotateY
    MirrorX  -> mirrorX
    MirrorY  -> mirrorY


data Edge = A | B | C | D | E | F | G | H | I | J | K | L deriving Show

unOp :: Op -> [Edge] -> [Edge]
unOp op edges = case op of
  Invert   -> unOpInvert edges
  RotateXZ -> map unOpRotateXZ edges
  RotateX  -> map unOpRotateX  edges
  RotateY  -> map unOpRotateY  edges
  MirrorX  -> unOpInvert $ map unOpMirrorX  edges
  MirrorY  -> unOpInvert $ map unOpMirrorY  edges

unOpInvert :: [Edge] -> [Edge]
unOpInvert [] = []
unOpInvert (a:b:c:d) = a : c : b : unOpInvert d
unOpInvert _ = undefined

unOpRotateXZ :: Edge -> Edge
unOpRotateXZ a = case a of
  D -> A
  H -> B
  L -> C
  E -> D
  A -> E
  C -> F
  K -> G
  I -> H
  B -> I
  G -> J
  J -> K
  F -> L

unOpRotateX :: Edge -> Edge
unOpRotateX a = case a of
  C -> A
  G -> B
  K -> C
  H -> D
  D -> E
  B -> F
  J -> G
  L -> H
  A -> I
  F -> J
  I -> K
  E -> L

unOpRotateY :: Edge -> Edge
unOpRotateY a = case a of
  E -> A
  D -> B
  H -> C
  L -> D
  I -> E
  A -> F
  C -> G
  K -> H
  F -> I
  B -> J
  G -> K
  J -> L

unOpMirrorX :: Edge -> Edge
unOpMirrorX a = case a of
  E -> F
  F -> E
  L -> J
  J -> L
  H -> G
  G -> H
  D -> B
  B -> D
  a -> a
  
unOpMirrorY :: Edge -> Edge
unOpMirrorY a = case a of
  A -> C
  C -> A
  F -> G
  G -> F
  E -> H
  H -> E
  I -> K
  K -> I
  a -> a

vertexIndex :: (Int, Int, Int) -> Edge -> ((Int, Int, Int), Axis)
vertexIndex (x, y, z) a = case a of
  A -> ((x, y, z),         X)
  B -> ((x + 1, y, z),     Y)
  C -> ((x, y + 1, z),     X)
  D -> ((x, y, z),         Y)
  E -> ((x, y, z),         Z)
  F -> ((x + 1, y, z),     Z)
  G -> ((x + 1, y + 1, z), Z)
  H -> ((x, y + 1, z),     Z)
  I -> ((x, y, z + 1),     X)
  J -> ((x + 1, y, z + 1), Y)
  K -> ((x, y + 1, z + 1), X)
  L -> ((x, y, z + 1),     Y)

