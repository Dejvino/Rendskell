--
--	RENDERER
--
-- 3D rendering module
--
-- (c) David Nemecek 2008
--
module Renderer where

import System.Environment
import System.IO
import Image
import Geometry

-- 	Common renderer types
type Objects = [Set]	-- array of sets (order matters)

data LightType = AmbientLight | DirectionalLight | SpotLight
type Light = (LightType, Point3, Vector3, Float, Float, Color) -- type, pos, dir, radius, pwr, color
type Lights = [Light]

data ProjectionType = Parallel | Perspective
type Camera = (Point3, Vector3, Vector3, ProjectionType, String, Integer, Integer) -- pos, dirX, dirY, ptype, file width height
type Cameras = [Camera]

type Scene = (Cameras, Lights, Objects)

getCamerasScene :: Scene -> Cameras
getCamerasScene (c,_,_) = c

getLightsScene :: Scene -> Lights
getLightsScene (_,l,_) = l

getObjectsScene :: Scene -> Objects
getObjectsScene (_,_,o) = o

nullScene :: Scene
nullScene = ([], [], [])

-- 	Default scene configuration
background_color :: Pixel
background_color = (50,50,80)

-- distance of the rendering canvas for default perspective cameras
freeDist :: Float
freeDist = pi/2

-- canvas stretching settings
renderStep :: Float
renderStep = 4

-- default ambient light settings
ambientLight :: Vector3
ambientLight = (1,-1,1)

-- shadow settings
shadesteps = 10


-- 	Render functions

--
-- 	computeLight
--
-- computes pixel color for given intersecion and light
type ComputeLight = Ray -> Intersection -> Scene -> Light -> Color
computeLight :: ComputeLight
computeLight r i s light@(lt, _, _, _, _, _) = case lt of
	AmbientLight -> computeAmbientLight r i s light
	DirectionalLight -> computeDirectionalLight r i s light
	SpotLight -> computeSpotLight r i s light

computeAmbientLight :: ComputeLight
computeAmbientLight _ _ _ light@(_, _, _, _, pwr, col) = mulColor col pwr

computeDirectionalLight :: ComputeLight
computeDirectionalLight ray@(base, rdir) intersection@(start, end, normal, color) scene@(cams,lights,objs) light@(_, _, dir, _, pwr, col) = let
	point = pointRay ray start
        points = [ pointRay (point, normal) (x * 0.01) | x <- [1..shadesteps] ]
        lgh = (multiplyVector3 dir (-1))
        lightVal = (foldr (\p -> (+)(if null (getIntersections (p, lgh) objs) then 1 else 0)) 0 points) / shadesteps
	in mulColor (mulColor (mulColor col (((angleVector3 dir normal)/pi)^2)) lightVal) pwr

computeSpotLight :: ComputeLight
computeSpotLight _ _ _ _ = (0,0,0)

--
--	computePixel
--
-- computes pixel color values from light and normal vectors
computePixel :: Ray -> Intersection -> Scene -> Pixel
computePixel ray@(base, dir) intersection@(start, end, normal, color) scene@(cams,lights, objs) = let
	point = pointRay ray start
	lgh = (multiplyVector3 ambientLight (-1))
	light = foldr (\l -> (addColor (computeLight ray intersection scene l))) (0,0,0) lights
	in translatePixel (mulColors color light)

--
--	renderPixel
--
-- sends single ray and gets first intersection
renderPixel :: Ray -> Scene -> Pixel
renderPixel ray@(base, dir) scene@(cams, lights, objs) = 
	let	intersections = getIntersections ray objs
		colision = head intersections
	in if not (null intersections)
		then computePixel ray colision scene
		else background_color

--
-- 	renderImage
--
-- helper function used to select actual renderer based on projection type
-- actual renderers then create rays and return computed pixel values
renderImage :: Scene -> Camera -> Pixmap
renderImage s cam@(_, _, _, ptype, _, _, _) = case ptype of
	Parallel -> renderImageParallel s cam
	Perspective -> renderImagePerspective s cam

-- shared function type for rendering function
type RenderImage = Scene -> Camera -> Pixmap

-- parallel projection
renderImageParallel :: RenderImage
renderImageParallel scene cam@(eyePos, eyeDirX, eyeDirY, _, _, width, height) = [ let
	newPoint = moveVector3 (multiplyVector3 eyeDirY yy) (multiplyVector3 eyeDirX xx)
	xx = (fromInteger x - (fromInteger width)/2) * renderStep / fromInteger width
	yy = (fromInteger y - (fromInteger height)/2) * renderStep / fromInteger height
	base = movePoint3 eyePos newPoint
	dir = crossVector3 eyeDirX eyeDirY
	in renderPixel (base, dir) scene | y <- [0..height-1], x <- [0..width-1] ]	

-- projection with perspective
renderImagePerspective :: RenderImage
renderImagePerspective scene cam@(eyePos, eyeDirX, eyeDirY, _, _, width, height) = [ let
        newPoint = moveVector3 (multiplyVector3 eyeDirY yy) (multiplyVector3 eyeDirX xx)
        xx = (fromInteger x - (fromInteger width)/2) * renderStep / fromInteger width
        yy = (fromInteger y - (fromInteger height)/2) * renderStep / fromInteger height
        base = movePoint3 eyePos newPoint
        viewDir = crossVector3 eyeDirX eyeDirY
	rayDir = pointRay (newPoint, viewDir) freeDist
        in renderPixel (eyePos, rayDir) scene | y <- [0..height-1], x <- [0..width-1] ]

--
-- 	render
--
-- main rendering function which takes scene + camera, renders it and then saves it
render :: Scene -> Camera -> IO ()
render scene cam@(_,_,_,_,file,width,height) = do
	putStr (file ++ "...")
	saveImage  file (fromInteger width) (fromInteger height) (renderImage scene cam)
	putStrLn "done"

