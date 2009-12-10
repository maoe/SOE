module Main where
import SimpleGraphics

fillTri :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size =
  drawInWindow w $ withColor Blue $
    polygon [ (x, y), (x + size, y), (x, y - size), (x, y) ]

minSize = 1

sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size
  | size <= minSize = fillTri w x y size
  | otherwise       = let size2 = size `div` 2
                      in do sierpinskiTri w x y size2
                            sierpinskiTri w x (y - size2) size2
                            sierpinskiTri w (x + size2) y size2

main :: IO ()
main = runGraphics $ do
  w <- openWindow "Sierpinski's Triangle" (500, 500)
  sierpinskiTri w 50 450 450
  spaceClose w