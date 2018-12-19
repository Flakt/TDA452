module Main where

import Tsuro

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action)
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
    
    let board = boardNew @@= (0,0) $ tile' [0,1,2,3,4,5,6,7]
    grid <- displayBoard board 

    containerAdd window grid

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
displayState game = undefined -- TODO maybe shouldnt be used

displayBoard :: Board -> IO Grid
displayBoard b = do
    let coords = [(x,y) | y <- [0..5], x <- [0..5]]
    let tiles' = zip (concat $ tiles b) coords

    grid <- gridNew

    mapM_ (attachTile grid) tiles' -- maybe works?

    return grid

-- | Attaches a tile to a grid
attachTile :: Grid -> (Maybe Tile, Pos) -> IO ()
attachTile grid (tile, (x,y)) = do
    img <- tileToImg tile 
    gridAttach grid img x y 1 1    

tileToImg :: Maybe Tile -> IO Image
tileToImg tile = do
    fp <- tileToFilepath tile
    imageNewFromFile fp

-- | Given an normalized [Connection] as string
tileToFilepath :: Maybe Tile -> IO String
tileToFilepath tile = do
    userEntry <- getRealUserID >>= getUserEntryForID
    let home = homeDirectory userEntry
    let id = maybe "blank" (filter (/=',') . dropFirstAndLast . show . toList . normalize) tile
    return (home ++ "/Pictures/tsuro-tiles/" ++ 
            id ++ ".png")

dropFirstAndLast :: String -> String
dropFirstAndLast s = drop 1 $ take (-1 + length s) s