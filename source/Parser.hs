--
--	PARSER
--
-- Scene description parser module
--
-- (c) David Nemecek 2008
--
module Parser where

import Char
import System.Environment
import IO
import Image
import Geometry
import Renderer

-- 	Common data types
type FileData = [String]

--
--	parserOpenFile
--
-- loads data from Rendskell Scene Description (.rsd) file
parserOpenFile :: String -> IO FileData
parserOpenFile name = do
	text <- readFile name
	let fileData = lines text
	if (head fileData) == "Rendskell"
		then 	putStrLn "File type OK" >> return (tail fileData)
		else 	putStrLn "File type NOT OK!" >> return []
--
-- 	getFlprm
--
-- helper function - gets n-th Float value from string array
getFlprm :: Int -> [String] -> Float
getFlprm id params = read (params !! id)::Float

--
--	getInprm
--
-- helper function - gets n-th Integer value from string array
getInprm :: Int -> [String] -> Integer
getInprm id params = read (params !! id)::Integer

--
--	parserBuildScene
--
-- creates scene described by command and params
parserBuildScene :: Scene -> String -> [String] -> Scene

-- create sphere
parserBuildScene scene@(cams, lights, objs) "Sphere" params = let
	action = case head params of
		"add" -> Add
		"sub" -> Sub
		"null" -> Null
	posX = getFlprm 1 params
	posY = getFlprm 2 params
	posZ = getFlprm 3 params
	rad  = getFlprm 4 params
	colR = getFlprm 5 params
	colG = getFlprm 6 params
	colB = getFlprm 7 params
	in (cams, lights, (action, (Sphere (posX,posY,posZ) rad (colR,colG,colB))) : objs)

-- create camera
parserBuildScene scene@(cams, lights, objs) "Camera" params = let
	pos = (getFlprm 0 params, getFlprm 1 params, getFlprm 2 params)
	dirX = (getFlprm 3 params, getFlprm 4 params, getFlprm 5 params)
	dirY = (getFlprm 6 params, getFlprm 7 params, getFlprm 8 params)
	ptype = case params !! 9 of
		"parallel" -> Parallel
		"perspective" -> Perspective
	file = params !! 10
	width = getInprm 11 params
	height = getInprm 12 params
	in ((pos, dirX, dirY, ptype, file, width, height) : cams, lights, objs)

-- create ambient light
parserBuildScene scene@(cams, lights, objs) "Light" ("ambient":params) = let
	col = (getFlprm 1 params, getFlprm 2 params, getFlprm 3 params)
	pwr = getFlprm 0 params
        in (cams, (AmbientLight, (0,0,0), (0,0,0), 0, pwr, col):lights, objs)

-- create directional light
parserBuildScene scene@(cams, lights, objs) "Light" ("directional":params) = let
	dir = (getFlprm 0 params, getFlprm 1 params, getFlprm 2 params)
	pwr = getFlprm 3 params
	col = (getFlprm 4 params, getFlprm 5 params, getFlprm 6 params)
	in (cams, (DirectionalLight, (0,0,0), dir, 0, pwr, col):lights, objs)

-- end scene description
parserBuildScene scene "/Scene" _ = scene


--
--	parserGetScene
--
-- parses file and processes it
parserHandleScene :: Scene -> FileData -> IO Scene
parserHandleScene scene (line:fileData) = do
	if null line || (head line) == '#'
		then 	(parserHandleScene scene fileData) -- comment
		else let
			cmd = head (words line)
			params = tail (words line)
			newscene = (parserBuildScene scene cmd params)
			samecams = length (getCamerasScene newscene) == length (getCamerasScene scene)
			samelights = length (getLightsScene newscene) == length (getLightsScene scene)
			sameobjs = length (getObjectsScene newscene) == length (getObjectsScene scene)
			in if samecams && samelights && sameobjs 
				then 	return scene -- scene hasn't changed -> nothing more to do
				else 	(parserHandleScene newscene fileData)

