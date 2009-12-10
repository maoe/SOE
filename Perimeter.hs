module Perimeter
  ( perimeter
  , module Shape
  ) where

import Shape
import Data.List

perimeter :: Shape -> Double
perimeter (Rectangle s1 s2)  = 2*(s1 + s2)
perimeter (RtTriangle s1 s2) = s1 + s2 + sqrt (s1^2 + s2^2)
perimeter (Polygon vs)       = foldl1' (+) (sides vs)
perimeter (Ellipse r1 r2)    = ellipsePerim (r1 `max` r2) (r1 `min` r2)
  where ellipsePerim r1 r2 = let e = sqrt (r1^2 - r2^2)/r1
                                 s = scanl aux (0.25*e^2) [2..]
                                 aux s i = nextEl e s i
                                 sSum = foldl1' (+) (takeWhile (> epsilon) s)
                             in 2 * r2 * pi * (1 - sSum)

sides :: [Vertex] -> [Side]
sides vs = zipWith distBetween vs (cycle vs)

nextEl :: Double -> Double -> Double -> Double
nextEl e s i = s * (2*i - 1) * (2*i - 3) * (e^2)/(4*i^2)

epsilon :: Double
epsilon = 0.0001
