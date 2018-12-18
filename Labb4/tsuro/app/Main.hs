module Main where

import Tsuro

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Graphics.UI.Gtk.Layout.Grid
import System.Posix.User


main :: IO ()
main = do
    void initGUI
    window <- windowNew
    set window [ windowTitle         := "Calculator"
               , windowResizable     := False
               , windowDefaultWidth  := 640
               , windowDefaultHeight := 640 ]
    
    board <- gridNew

    img1 <- loadTileImage "blank"
    img2 <- loadTileImage "blank"
    img3 <- loadTileImage "blank"
    img4 <- loadTileImage "01234567"
    img5 <- loadTileImage "blank"

    let toBoard = gridAttach board

    toBoard img1 0 0 1 1
    toBoard img2 0 1 1 1
    toBoard img3 1 0 1 1
    toBoard img5 1 1 1 1
    toBoard img4 1 1 1 1

    containerAdd window board

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    widgetShowAll window
    mainGUI

-- | Creates a new element based on a game state, containing 
--   - a grid of images
--   - the current player's hand
--   - buttons to rotate or place
displayState :: Game -> IO VBox
displayState game = undefined -- TODO

displayBoard :: Board -> IO Grid
displayBoard b = do
    let coords = [(x,y) | y <- [0..5], x <- [0..5]]
    let tiles' = zip (concat $ tiles b) coords

    grid <- gridNew

    let _ = map (attachTile grid) tiles' -- maybe works?

    return grid

-- | Given a grid, attaches an image representing the given tile
attachTile :: Grid -> (Maybe Tile, Pos) -> IO ()
attachTile grid (tile, (x,y)) = do
    img <- tileToImg tile 
    gridAttach grid img x y 1 1    

tileToImg :: Maybe Tile -> IO Image
tileToImg tile = loadTileImage id
    where
        id = undefined -- TODO

-- | Given an id*, returns an Image
-- *format: for all connections
--          fst < snd
--          sort on fst
--          to string
loadTileImage :: String -> IO Image 
loadTileImage id = do
    ue <- getRealUserID >>= getUserEntryForID
    let home = homeDirectory ue
    imageNewFromFile $ home ++ "/Pictures/tsuro-tiles/" ++ id ++".png"
