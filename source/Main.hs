--
--	MAIN
--
-- Program execution start point
--
-- (c) David Nemecek 2008
--
module Main (main) where

import Char
import System.Environment
import System.IO
import Utils
import Image
import Geometry
import Renderer
import Parser

-- Build configuration
version :: String
version = "0.0.8"

--
--	renderCameras
--
-- takes scene description and renders all listed cameras
renderCameras :: Scene -> IO ()
renderCameras scene@((c:cams), lights, objs) = do
	render scene c
	renderCameras (cams, lights, objs)
renderCameras ([], _, _) = return ()

--
--	handleFiles
--
-- takes list of filenames, parses each scene description and renders it
handleFiles :: [String] -> IO ()
handleFiles [] = putStrLn "Finished!"
handleFiles (x:s) = do
	putStrLn ("-- Processing " ++ x)
	fileData <- parserOpenFile x
	if fileData == []
		then putStrLn "No data"
		else do	scene <- (parserHandleScene nullScene fileData)
			if length (getCamerasScene scene) == 0
				then putStrLn "No cameras!"
				else renderCameras scene
	handleFiles s		

--
--	main
--
-- main program
main :: IO ()
main = do
	putStrLn ("Rendskell v" ++ version)

	args <- getArgs

	if null args
		then putStrLn "No scene filenames on input, exiting..."
		else handleFiles args
