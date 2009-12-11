module Region
  ( Region ( Shape
           , Translate
           , Scale
           , Complement
           , Union
           , Intersect
           , Empty )
  , Coordinate
  , containsS, containsR
  , module Shape
  ) where

import Control.Applicative
import Data.Complex
import Data.Monoid

import Shape

data Region = Shape Shape               -- ^ primitive shape
            | Translate Vector Region   -- ^ translated region
            | Scale Vector Region       -- ^ scaled region
            | Complement Region         -- ^ inverse of region
            | Region `Union` Region     -- ^ union of regions
            | Region `Intersect` Region -- ^ intersection of regions
            | Empty                     -- ^ empty region
              deriving Show

instance Monoid Region where
  mempty  = Empty
  mappend = Union

infixr 5 `Union`
infixr 6 `Intersect`

type Vector = Complex Double

difference :: Region -> Region -> Region
difference = Intersect . Complement

data Coordinate = Coordinate

containsS = undefined

containsR = undefined

-- examples
oneCircle = Shape $ Ellipse 1 1
manyCircles = [ Translate (x :+ 0) oneCircle | x <- [0,2..] ]
fiveCircles  = mconcat $ take 5 manyCircles
fiveCircles' = Shape (Rectangle 1 5) `Intersect` mconcat (Complement <$> manyCircles)