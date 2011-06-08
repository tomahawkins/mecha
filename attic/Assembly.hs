module Language.Mecha.Assembly
  ( Assembly
  , part
  , assemble
  , Scene
  , Camera (..)
  , view
  , animate
  ) where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Digest.CRC32
import Language.Mecha.Solid
import Language.Mecha.Types
import System.Directory
import System.Process
import Text.Printf

-- | An Assembly holds all the parts and sub-assemblies.
data Assembly
  = Assembly [Assembly]
  | Part Solid
  | Label String Assembly
  deriving Eq

-- | General assembly.
class Assemble a where assemble :: a -> Assembly

instance Assemble Assembly where assemble = id
instance Assemble Solid    where assemble = Part
instance Assemble a => Assemble [a] where assemble = Assembly . map assemble

-- | A general model transformer.
class    SMap a        where smap :: (Solid -> Solid) -> a -> a
instance SMap Solid    where smap = ($)
instance SMap Assembly where
  smap f a = case a of
    Assembly a -> Assembly $ map (smap f) a
    Part     a -> Part $ smap f a
    Label  n a -> Label n $ smap f a

instance Colorable Assembly where color c = smap . color c

instance Moveable  Assembly where
  move    a = smap . move    a
  rotateX a = smap . rotateX a
  rotateY a = smap . rotateY a
  rotateZ a = smap . rotateZ a

instance Scaleable Assembly where scale v = smap . scale v

-- | A Scene is a light position, camera configuration, and an assembly.
type Scene = (Camera, Asm)

-- | Defines a camera configuration.
data Camera
  = Orthographic               -- ^ Orthographgic projection at the origin with a radius.
  | Perspective                -- ^ Perspective projection given a camera location and a target.
  deriving Eq

-- | Renders 3 orthographic views and 1 perspective view and creates a little html page or the images.  Assembly should be within 1 of origin.
view :: FilePath -> Int -> Int -> Asm -> IO ()
view f h w a = do
  writeFile (f ++ ".html") $ unlines
    [ printf "<table border=\"1\">"
    , printf "<tr><td><img src=\"%sTop.png\"/></td><td><img src=\"%sPersp.png\"/></td></tr>\n" f f
    , printf "<tr><td><img src=\"%sFront.png\"/></td><td><img src=\"%sRight.png\"/></td></tr>\n" f f
    , printf "</table>"
    ]
  render (f ++ "Top")   h w Orthographic $ rotateX (pi/2) a
  render (f ++ "Front") h w Orthographic $ a
  render (f ++ "Right") h w Orthographic $ rotateZ (-pi/2) a
  render (f ++ "Persp") h w Perspective  $ moveY 1 $ rotateX (pi/4) $ rotateZ (-pi/6) a

-- | Renders a MPEG movie with POVRay and ffmpeg given a file name (w/o file extension), heigth, width, frames-per-second, and a list of scenes.
animate :: FilePath -> Int -> Int -> Int -> [Scene] -> IO ()
animate file h w fps scenes = do
  sequence_ [ printf "[ %d of %d ]\n" i n >> render (printf "%s%05d" file i) h w camera asm | (i, (camera, asm)) <- zip [1 .. n] scenes ]
  rm $ file ++ ".mpg"
  readProcess "ffmpeg" ["-sameq", "-i", file ++ "%05d.png", "-r", show fps, file ++ ".mpg"] ""
  sequence_ [ rm $ printf "%s%05d.png" file i | i <- [1 .. n] ]
  where
  n = length scenes

-- | Renders a scene.
render :: String -> Int -> Int -> Camera -> Asm -> IO ()
render file h w camera (Asm a) = do
  ln image link
  a <- doesFileExist image
  when (not a) $ do
    writeFile (file ++ ".pov") povray'
    readProcess "povray" ["-D", "-V", "+H" ++ show h, "+W" ++ show w, "+I" ++ file ++ ".pov", "+O" ++ image] ""
    --rm $  file ++ ".pov"
    return ()
  where
  checksum = printf "%08X" $ crc32 $ BS.pack $ show (h, w, povray')
  image = checksum ++ ".png"
  link  = file     ++ ".png"
  r :: Double
  r = fromIntegral w / fromIntegral h
  povray' :: String
  povray' = unlines
    [ "#include \"colors.inc\""
    , "background { color White }"
    , printf "light_source { <100, 100, -100> color White }"
    , case camera of
        Perspective  -> printf "camera { perspective location <0, 0, 0> right x*%f direction <0, 0, 1> }" r
        Orthographic -> printf "camera { orthographic location <0,0,-100> up y*1 right x*%f }" r
    ] ++ concatMap povray a
 
rm  :: FilePath -> IO ()
rm f = system ("rm -f " ++ f) >> return ()

ln :: FilePath -> FilePath -> IO ()
ln a b = system ("ln -f -s " ++ a ++ " " ++ b) >> return ()

