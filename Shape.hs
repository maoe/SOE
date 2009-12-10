{-# LANGUAGE BangPatterns #-}
module Shape
  ( Shape (..)
  , Radius, Side, Vertex
  , square, circle, distBetween, area
  , module Data.Complex
  ) where
import Data.List (unfoldr)
import Data.Complex

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
             deriving Show

type Radius = Double
type Side   = Double
type Vertex = Complex Double

square :: Side -> Shape
square s = Rectangle s s

circle :: Radius -> Shape
circle r = Ellipse r r

rectangle :: Side -> Side -> Shape
rectangle w h = Polygon [ 0 :+ 0, w :+ 0
                        , w :+ h, 0 :+ h ]

-- Exercise 2.1
rtTriangle :: Side -> Side -> Shape
rtTriangle w h = Polygon [ 0 :+ 0, w :+ 0, 0 :+ h ]

-- Exercise 2.2
regularPolygon :: Int -> Side -> Shape
regularPolygon n l = Polygon $ unfoldr (uncurry go) (n, 0)
  where go  0 _ = Nothing
        go !i z = let lz = l :+ 0
                      theta = 2 * pi / fromIntegral n
                      z' = z + rotate (fromIntegral (n-i) * theta) lz
                  in  Just (z, (i-1, z'))
        rotate r z = z * (cos r :+ sin r)

area :: Shape -> Double
area (Rectangle s1 s2)  = s1*s2
area (RtTriangle s1 s2) = s1*s2/2
area (Ellipse r1 r2)    = pi*r1*r2
area (Polygon (v1:vs))  = polyArea vs
  where polyArea :: [Vertex] -> Double
        polyArea (v2:v3:vs') = triArea v1 v2 v3 + polyArea (v3:vs')
        polyArea _           = 0

triArea :: Vertex -> Vertex -> Vertex -> Double
triArea v1 v2 v3 = let a = distBetween v1 v2
                       b = distBetween v2 v3
                       c = distBetween v3 v1
                       s = (a + b + c) / 2
                   in sqrt $ s * (s - a) * (s - b) * (s - c)

distBetween :: Vertex -> Vertex -> Double
distBetween (x1 :+ y1) (x2 :+ y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

isConvex :: Shape -> Bool
isConvex (Polygon vs) = let theta = 2*pi/fromIntegral (length vs)
                        in undefined
isConvex _            = True

l = foldl (flip (const succ)) 0