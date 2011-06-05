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
  clearColor $= Color4 1 1 1 0
  displayCallback $= do
    putStrLn "Display Callback"
    redraw
  reshapeCallback $= (Just $ \ size -> putStrLn $ "Reshape Callback: " ++ show size)
  keyboardMouseCallback $= (Just $ \ key keyState mods pos -> do
    print key
    print keyState
    print mods
    print pos)
  mainLoop

redraw :: IO ()
redraw = do
  clear [ColorBuffer, DepthBuffer]
  swapBuffers

