module Language.Mecha.Viewer
  ( viewer
  ) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL hiding (init, Color)
import qualified Graphics.UI.SDL as SDL

import Language.Mecha.OpenGL

data State = State
  { leftButton
  , middleButton
  , rightButton  :: Bool
  , theta
  , phi
  , scale'
  , theta'
  , phi' :: Float
  , x'
  , y' :: Int
  , i
  , j
  , i'
  , j' :: Float
  , running :: Bool
  } deriving Show

initState = State
  { leftButton  = False
  , middleButton = False
  , rightButton = False
  , theta  = 45 * pi / 180
  , phi    = 30 * pi / 180
  , scale' = 0.4
  , theta' = 0
  , phi'   = 0
  , x'     = 0
  , y'     = 0
  , i      = 0
  , j      = 0
  , i'     = 0
  , j'     = 0
  , running = True
  }

type Model = IO ()

viewer :: Model -> IO ()
viewer model = do
  SDL.init [InitVideo]
  setCaption "ModelView" "ModelView"
  glSetAttribute glRedSize   8
  glSetAttribute glGreenSize 8
  glSetAttribute glBlueSize  8
  glSetAttribute glAlphaSize 8
  glSetAttribute glDepthSize 24
  glSetAttribute glDoubleBuffer 1
  setView 600 400
  cullFace  $= Nothing
  shadeModel $= Smooth
  normalize $= Enabled

  position (Light 0) $= Vertex4 1 1 1 0
  ambient  (Light 0) $= Color4 0.3 0.3 0.3 1
  diffuse  (Light 0) $= Color4 1 1 1 1
  --specular (Light 0) $= Color4 0 0 0 1
  specular (Light 0) $= Color4 1 1 1 1
  lightModelAmbient $= Color4 0.2 0.2 0.2 1
  lighting $= Enabled
  light (Light 0) $= Enabled
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  materialSpecular FrontAndBack $= Color4 1 1 1 1
  materialEmission FrontAndBack $= Color4 0 0 0 1
  materialShininess FrontAndBack $= 30

  clearColor $= Color4 1 1 1 0
  clearDepth $= 1
  depthFunc  $= Just Less
  depthMask $= Enabled
  loop model initState
  quit

setView :: Int -> Int -> IO ()
setView w h = do
  setVideoMode w h 16 [OpenGL, Resizable] >> return ()
  matrixMode $= Projection
  loadIdentity
  let r = (fromIntegral w / fromIntegral h)
  frustum (-r * 0.1) (r * 0.1) (-0.1) 0.1 0.1 100000
  matrixMode $= Modelview 0
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

redraw :: Model -> State -> IO ()
redraw model state = do
  clear [ColorBuffer, DepthBuffer] 
  loadIdentity
  stateView state
  lighting $= Disabled
  orign
  lighting $= Enabled
  model
  flush
  glSwapBuffers

stateView :: State -> IO ()
stateView state = do
  translate3 0 0 (-1)
  rotate3 (phi state)   1 0 0
  rotate3 (theta state) 0 1 0
  rotate3 (-pi / 2) 1 0 0
  rotate3 (-pi / 2) 0 0 1
  scale3  (scale' state) (scale' state) (scale' state)








loop :: Model -> State -> IO ()
loop model state = do
  event <- pollEvent
  state <- handler event model state
  when (event /= Quit) $ loop model state

handler :: Event -> Model -> State -> IO State
handler event model state = case event of
  NoEvent         -> return state
  VideoExpose     -> redraw model state >> return state
  VideoResize x y -> setView x y >> return state
  event -> case nextState event state of
    Nothing -> return state
    Just state -> redraw model state >> return state

nextState :: Event -> State -> Maybe State
nextState event state = case event of
  MouseMotion x y _ _ | middleButton state -> Just state
    { phi   = phi' state   + 0.01 * fromIntegral (fromIntegral y - y' state)
    , theta = theta' state + 0.01 * fromIntegral (fromIntegral x - x' state)
    }
  MouseMotion x _ _ _ | leftButton  state -> Just state { i = i' state + 0.01 * fromIntegral (fromIntegral x - x' state) }
  MouseMotion x _ _ _ | rightButton state -> Just state { j = j' state + 0.01 * fromIntegral (fromIntegral x - x' state) }

  MouseButtonDown x y ButtonMiddle -> Just state
    { leftButton   = False
    , middleButton = True
    , rightButton  = False
    , x' = fromIntegral x
    , y' = fromIntegral y
    , phi' = phi state
    , theta' = theta state
    }

  MouseButtonDown x y ButtonLeft -> Just state
    { leftButton   = True
    , middleButton = False
    , rightButton  = False
    , x' = fromIntegral x
    , y' = fromIntegral y
    , i' = i state
    , j' = j state
    }

  MouseButtonDown x y ButtonRight -> Just state
    { leftButton   = False
    , middleButton = False
    , rightButton  = True
    , x' = fromIntegral x
    , y' = fromIntegral y
    , i' = i state
    , j' = j state
    }

  MouseButtonUp _ _ ButtonLeft   -> Just state { leftButton   = False }
  MouseButtonUp _ _ ButtonMiddle -> Just state { middleButton = False }
  MouseButtonUp _ _ ButtonRight  -> Just state { rightButton  = False }
  MouseButtonDown _ _ ButtonWheelUp   -> Just state { scale' = scale' state * 1.2 }
  MouseButtonDown _ _ ButtonWheelDown -> Just state { scale' = scale' state / 1.2 }
  _ -> Nothing

darkGray  = color3 0.4 0.4 0.4
--lightGray = color3 0.7 0.7 0.7

orign :: IO ()
orign = do
  lineWidth $= 1
  renderPrimitive Lines $ do
    color3  0.7 0 0
    vertex3 0 0 0
    vertex3 inf 0 0
    color3  0 0.7 0
    vertex3 0 0 0
    vertex3 0 inf 0
    color3  0 0 0.7
    vertex3 0 0 0
    vertex3 0 0 inf
    darkGray
    vertex3 0 0 0
    vertex3 (-inf) 0 0
    vertex3 0 0 0
    vertex3 0 (-inf) 0
    vertex3 0 0 0
    vertex3 0 0 (-inf)
  where
  inf = 1e6
    
{-
plane :: Float -> Int -> Int -> IO ()
plane delta linesPerMajor totalMajors = do
  lineWidth $= 1
  renderPrimitive Lines $ line 1 (linesPerMajor - 1) delta
  where
  y = delta * fromIntegral (linesPerMajor * totalMajors)
  line :: Int -> Int -> Float -> IO ()
  line majorCount _ _ | majorCount > totalMajors = return ()
  line majorCount minorCount x | minorCount == 0 = do
    darkGray
    line' x
    line (majorCount + 1) (linesPerMajor - 1) (x + delta)
  line majorCount minorCount x = do
    lightGray
    line' x
    line majorCount (minorCount - 1) (x + delta)
  line' x = do
    vertex3   x    y  0
    vertex3   x  (-y) 0
    vertex3 (-x)   y  0
    vertex3 (-x) (-y) 0
    vertex3   y    x  0
    vertex3 (-y)   x  0
    vertex3   y  (-x) 0
    vertex3 (-y) (-x) 0
-}
{-
posZ0 :: Int -> Int -> IO (Maybe (Float,Float))
posZ0 x y = do
  ((x1,y1,z1),(x2,y2,z2)) <- unProject x y
  if z1 > 0 && z2 > 0 || z1 < 0 && z2 < 0 then return Nothing else do
    let r = abs z1 / abs (z2 - z1)
        x' = r * (x2 - x1) + x1
        y' = r * (y2 - y1) + y1
    return $ Just (x',y')
-}

