module SimpleGraphics
  ( spaceClose
  , module Graphics.SOE.Gtk
  ) where

import Graphics.SOE.Gtk

spaceClose :: Window -> IO ()
spaceClose w = do
  k <- getKey w
  if k == ' '
    then closeWindow w
    else spaceClose w
