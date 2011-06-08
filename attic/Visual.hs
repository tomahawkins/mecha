module Language.Mecha.Visual
  ( visual
  ) where

import Graphics.UI.GLUT

import Language.Mecha.Assembly

visual :: Asm -> IO ()
visual _ = do
  initialize "Mecha" []
  initialWindowSize   $= Size 800 600
  initialDisplayMode  $= [RGBAMode, WithDepthBuffer, DoubleBuffered]
  actionOnWindowClose $= MainLoopReturns
  createWindow "Mecha Visual"
  setView 800 600
  displayCallback $= redraw
  reshapeCallback $= (Just $ \ (Size w h) -> setView (fromIntegral w) (fromIntegral h))
  keyboardMouseCallback $= (Just $ \ key keyState mods pos -> do
    print key
    print keyState
    print mods
    print pos)

  position (Light 0) $= Vertex4 1 1 0 1

  ambient  (Light 0) $= Color4 0 0 0 1
  diffuse  (Light 0) $= Color4 1 1 1 1
  specular (Light 0) $= Color4 1 1 1 1

  lightModelAmbient $= Color4 0.2 0.2 0.2 1
  lighting $= Enabled
  light (Light 0) $= Enabled
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  materialSpecular FrontAndBack $= Color4 1 1 1 1
  materialEmission FrontAndBack $= Color4 0 0 0 1
  normalize  $= Enabled
  clearColor $= Color4 0.4 0.4 0.4 1
  clearDepth $= 1
  depthFunc  $= Just Less
  depthMask  $= Enabled
  cullFace   $= Nothing
  shadeModel $= Smooth
  mainLoop

setView :: Int -> Int -> IO ()
setView w h = do
  matrixMode $= Projection
  loadIdentity
  let r = (fromIntegral w / fromIntegral h)
  frustum (-r * 0.1) (r * 0.1) (-0.1) 0.1 0.1 100000
  matrixMode $= Modelview 0
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

redraw :: IO ()
redraw = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  -- XXX
  flush
  swapBuffers

