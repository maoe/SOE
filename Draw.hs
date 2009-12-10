module Draw
  ( inchToPixel, pixelToInch, intToDouble
  , xWin, yWin, trans, shapeToGraphic, spaceClose
  ) where
import Shape
import SimpleGraphics

inchToPixel :: Double -> Int
inchToPixel = round . (* 100)

pixelToInch :: Int -> Double
pixelToInch = (/ 100) . intToDouble

intToDouble :: Int -> Double
intToDouble = fromInteger . toInteger

xWin, yWin :: Int
(xWin, yWin) = (600, 500)

trans :: Vertex -> Point
trans (x :+ y) = ( xWin `div` 2 + inchToPixel x
                 , yWin `div` 2 - inchToPixel y )

shapeToGraphic :: Shape -> Graphic
shapeToGraphic (Rectangle s1 s2)  = let s12 = s1/2
                                        s22 = s2/2
                                    in  polygon $ map trans [ (-s12) :+ (-s22), (-s12) :+ s22
                                                            , s12 :+ s22, s12 :+ (-s22) ]
shapeToGraphic (Ellipse r1 r2)    = ellipse (trans $ (-r1) :+ (-r2)) (trans $ r1 :+ r2)
shapeToGraphic (RtTriangle s1 s2) = polygon $ map trans [0 :+ 0, s1 :+ 0, 0 :+ s2]
shapeToGraphic (Polygon vts)      = polygon $ map trans vts

type ColoredShape = (Color, Shape)

drawShapes :: Window -> [ColoredShape] -> IO ()
drawShapes w = mapM_ (uncurry go)
  where go c = drawInWindow w . withColor c . shapeToGraphic

-- examples
example1 = runGraphics $ do
  w <- openWindow "Drawing Shapes" (xWin, yWin)
  let sh1 = Rectangle 3 2
      sh2 = Ellipse 1 1.5
      sh3 = RtTriangle 3 2
      sh4 = Polygon [ (-2.5) :+ 2.5, (-1.5) :+ 2, (-1.1) :+ 0.2
                    , (-1.7) :+ (-1), (-3) :+ 0 ]
  drawShapes w $ [ (Red, sh1), (Blue, sh2) ]
  spaceClose w

example2 = runGraphics $ do
  w <- openWindow "Drawing Shapes" (xWin, yWin)
  let sh1 = Rectangle 3 2
      sh2 = Ellipse 1 1.5
      sh3 = RtTriangle 3 2
      sh4 = Polygon [ (-2.5) :+ 2.5, (-1.5) :+ 2, (-1.1) :+ 0.2
                    , (-1.7) :+ (-1), (-3) :+ 0 ]
  drawShapes w [ (Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4) ]
  spaceClose w

example3 = runGraphics $ do
  w <- openWindow "Bull's Eye" (xWin, yWin)
  let coloredCircles = zip [ Black, Blue, Green, Cyan, Red, Magenta, Yellow, White ]
                           [ circle c | c <- [2.4, 2.1 .. 0.3] ]
  drawShapes w coloredCircles
  spaceClose w