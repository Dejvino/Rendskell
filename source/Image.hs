--
--      IMAGE
--
-- Image manipulation module
--
-- (c) David Nemecek 2008
--
module Image ( PixVal, Pixel, Pixmap,
	getR, getG, getB,
	makePixel, setPixel, getPixel, translatePixel,
	pixelBlack, pixelWhite,
	saveImage ) where

import System.Environment
import System.IO
import Char
import Data.Bits

--
-- 	Data types
--

-- pixel value type
type PixVal = Int
-- image pixel
type Pixel = (PixVal, PixVal, PixVal)
-- array of pixels
type Pixmap = [Pixel]

-- 
-- 	Helper functions
--

pixelBlack :: Pixel
pixelBlack = (0,0,0)

pixelWhite :: Pixel
pixelWhite = (255,255,255)

--
-- makeString
--
-- converts array of bytes to string
makeString :: [Int] -> String
makeString = map chr

--
-- makeStringFromPixmap
--
-- exports pixmap to string for saving
makeStringFromPixmap :: Pixmap -> String
makeStringFromPixmap a = foldr (\ y x -> (exportPixel y) ++ x) [] a

--
-- makeWord
--
-- converts 2-byte number to array of 1-byte numbers
makeWord :: Int -> [Int]
makeWord i = [i .&. 0x00FF, shiftR (i .&. 0xFF00) 8 ]

--
-- tgaHeader
--
-- prepares header data for Targa image file format
tgaHeader :: Int -> Int -> [Int]
tgaHeader w h =  [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ makeWord w ++ makeWord h ++ [24, 0]

--
-- RGR functions
--
-- returns R / G / B value from pixel
getR :: Pixel -> PixVal
getR (v, _, _) = v

getG :: Pixel -> PixVal
getG (_, v, _) = v

getB :: Pixel -> PixVal
getB (_, _, v) = v

-- creates pixel
makePixel :: PixVal -> PixVal -> PixVal -> Pixel
makePixel r g b = (r, g, b)

-- changes Xth pixel in pixmap
setPixel :: Pixmap -> Int -> Pixel -> Pixmap
setPixel pixmap x pixel = (take x pixmap) ++ [pixel] ++ (drop (x+1) pixmap)

-- returns Xth pixel in pixmap
getPixel :: Pixmap -> Int -> Pixel
getPixel pixmap x = head (drop x pixmap)

-- exports pixel to string
exportPixel :: Pixel -> [Char]
exportPixel (r,g,b) = [chr b, chr g, chr r]

-- changes pixel from float to Int
translatePixel :: (Float, Float, Float) -> Pixel
translatePixel (r,g,b) = (floor ((min 1.0 r)*255), floor ((min 1.0 g)*255), floor ((min 1.0 b)*255))

--
--	Image modifications
--

getBrightness :: Pixel -> PixVal
getBrightness (r,g,b) = max r (max g b)

--
-- saveImage
--
-- creates Targa image and stores it
saveImage :: String -> Int -> Int -> Pixmap -> IO ()
saveImage name width height d = do
	h <- openBinaryFile (name ++ ".tga") WriteMode
	hPutStr h (makeString (tgaHeader width height) ++ (makeStringFromPixmap d))
	hClose h
