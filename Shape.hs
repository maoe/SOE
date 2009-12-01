{-# LANGUAGE BangPatterns #-}
module Share where
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
  where go 0 _ = Nothing
        go i z = let lz = l :+ 0
                     theta = 2 * pi / fromIntegral n
                     z' = z + rotate (fromIntegral (n-i) * theta) lz
                 in  Just (z, (i-1, z'))
        rotate r z = z * (cos r :+ sin r)