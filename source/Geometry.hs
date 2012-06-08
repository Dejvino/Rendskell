--
--	GEOMETRY
--
-- Module containing 3D geometry functions and types
--
-- (c) David Nemecek 2008
--
module Geometry where

import Utils

-- 	Data types
-- Points
type Point2 = (Float, Float) -- X, Y position
type Point3 = (Float, Float, Float) -- X, Y, Z position
-- Vectors
type Vector2 = Point2
type Vector3 = Point3
-- graphics
type Color = (Float, Float, Float) -- R G B
-- Space
type Ray = (Point3, Vector3) -- base + direction
type Intersection = (Float, Float, Vector3, Color) -- start, end, normal, pixel color

data Action = Null | Add | Sub deriving (Eq) -- set operation
data Shape = Sphere Point3 Float Color | Plane Point3 Vector3 Vector3 Float Float Float Float Color
	-- Sphere origin radius color
	-- Plane origin dirX dirY minX maxX minY maxY color

type Set = (Action, Shape)

-- Points
movePoint2 :: Point2 -> Point2 -> Point2
movePoint2 (x,y) (a,b) = (x+a, y+b)

movePoint3 :: Point3 -> Point3 -> Point3
movePoint3 (x,y,z) (a,b,c) = (x+a, y+b, z+c)

distancePoint2 :: Point2 -> Point2 -> Float
distancePoint2 (a,b) (x,y) = sqrt (u*u + v*v) where u = a-x ; v = b-y

distancePoint3 :: Point3 -> Point3 -> Float
distancePoint3 (a,b,c) (x,y,z) = sqrt (u*u + v*v + w*w) where u = a-x ; v = b-y ; w = c-z

-- Vectors
getx :: Vector3 -> Float
getx (v, _, _) = v
gety :: Vector3 -> Float
gety (_, v, _) = v
getz :: Vector3 -> Float
getz (_, _, v) = v

moveVector2 = movePoint2
moveVector3 = movePoint3

magnitudeVector2 = distancePoint2 (0,0)
magnitudeVector3 = distancePoint3 (0,0,0)

magSquaredVector3 :: Vector3 -> Float
magSquaredVector3 (x,y,z) = x*x + y*y + z*z

multiplyVector2 :: Vector2 -> Float -> Vector2
multiplyVector2 (x,y) f = (x*f, y*f)

multiplyVector3 :: Vector3 -> Float -> Vector3
multiplyVector3 (x,y,z) f = (x*f, y*f, z*f)

normalizeVector2 :: Vector2 -> Vector2
normalizeVector2 (x,y)
	| magnitudeVector2 (x,y) /= 0 = multiplyVector2 (x,y) (1 / magnitudeVector2 (x,y))
	| otherwise = (0,0)

normalizeVector3 :: Vector3 -> Vector3
normalizeVector3 (x,y,z) 
        | magnitudeVector3 (x,y,z) /= 0 = multiplyVector3 (x,y,z) (1 / magnitudeVector3 (x,y,z))
        | otherwise = (0,0,0)

dotVector3 :: Vector3 -> Vector3 -> Float
dotVector3 (x,y,z) (a,b,c) = x*a + y*b + z*c

crossVector3 :: Vector3 -> Vector3 -> Vector3
crossVector3 (x,y,z) (a,b,c) = (y*c - b*z, z*a - c*x, x*b - a*y)

angleVector3 :: Vector3 -> Vector3 -> Float
angleVector3 u v = acos ( (dotVector3 u v) / (magnitudeVector3 u * magnitudeVector3 v) )

-- Color
mulColor :: Color -> Float -> Color
mulColor (r,g,b) v = (r*v, g*v, b*v)

mulColors :: Color -> Color -> Color
mulColors (a,b,c) (d,e,f) = (a*d,b*e,c*f)

addColor :: Color -> Color -> Color
addColor (a,b,c) (d,e,f) = (min 1 (a+d), min 1 (b+e), min 1 (c+f))

-- Set
getActionSet :: Set -> Action
getActionSet (a,_) = a

getShapeSet :: Set -> Shape
getShapeSet (_,s) = s

-- Ray
pointRay :: Ray -> Float -> Point3
pointRay ray@(base, dir) t = movePoint3 base (multiplyVector3 dir t)

-- Intersection
getStartIntersection :: Intersection -> Float
getStartIntersection (v, _, _, _) = v

getEndIntersection :: Intersection -> Float
getEndIntersection (_, v, _,  _) = v

getNormalIntersection :: Intersection -> Vector3
getNormalIntersection (_, _, v, _) = v

getColorIntersection :: Intersection -> Color
getColorIntersection (_, _, _, v) = v

--
-- intersect
--
-- returns array of intersections created by ray and object set
intersect :: Ray -> Set -> [Intersection]
intersect ray@(base, dir) set@(action, shape@(Sphere pos radius color)) = 
	let	newPos = moveVector3 base (multiplyVector3 pos (-1))
		a = magSquaredVector3 dir
		b = 2 * dotVector3 dir newPos
		c = magSquaredVector3 newPos - radius^2
		r = roots a b c
		normal = normalizeVector3 (moveVector3 (pointRay ray (head r)) (multiplyVector3 pos (-1)))
	in if (length r /= 0) then [(head r, head (tail r), normal, color)] else []

intersect ray@(base, dir) set@(action, shape@(Plane pos dirX dirY minX maxX minY maxY col)) = [] -- todo

--
-- getIntersections
--
-- returns array of intersections created by ray and array of object sets
getIntersections :: Ray -> [Set] -> [Intersection]
getIntersections _ [] = []
getIntersections ray ((x@(action,shape)):s) = let
	i = intersect ray x
	sorted = getIntersections ray s
	in if null i
		then sorted
		else case action of
			Null -> sorted
			Add -> let
				start = getStartIntersection (head i) -- todo: add more intersections once they are returned in some further version
				left = filter (\t -> (getStartIntersection t) < start) sorted
				right = filter (\t -> (getStartIntersection t) >= start) sorted
				in if start > 0
					then left ++ (head i) : right
					else left ++ right
			Sub -> subIntersection i sorted

--
-- subIntersection
--
-- removes one array of intersections from another one
subIntersection :: [Intersection] -> [Intersection] -> [Intersection]
subIntersection [] s = s
subIntersection _ [] = []
subIntersection (i@(start,end,normal,color):j) s = let
	purged = filter (\t@(tstart,tend,_,_) -> not ( (tstart >= start) && (tend <= end))) s
	purgedBack = map (\t@(a,tend,c,d) -> if ((tend > start) && (tend < end)) then (a,start,c,d) else t) purged
	purgedAll = map (\t@(tstart,b,c,d) -> if ((tstart > start) && (tstart < end)) then (end,b,multiplyVector3 normal (-1),d) else t) purgedBack
	in subIntersection j purgedAll
