--
--	UTILS
--
-- Utility functions
--
-- (c) David Nemecek 2008
--
module Utils where

--
--	simplesort
--
-- simple insertion sort
simplesort :: Ord a => [a] -> [a]
simplesort [] = []
simplesort (x:s) = (takeWhile (x>) sorted) ++ x : (dropWhile (x>) sorted) where sorted = simplesort s

--
--	roots
--
-- returns array of roots of quadratic equasion
roots :: Float -> Float -> Float -> [Float]
roots a b c = let
		d = (b*b - 4*a*c)
		x1 = (- sqrt d - b) / (2*a)
		x2 = (sqrt d - b) / (2*a)
		in if (d < 0) then [] else [min x1 x2, max x1 x2]

