module Main where

import Tsuro


import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action)
import Graphics.UI.Gtk.Layout.Grid
import System.Directory
import System.Random

main :: IO ()
main = do
    void initGUI
    window <- windowNew
    set window [ windowTitle         := "Calculator"
               , windowResizable     := False]
    
    --let (deck,_) = shuffle (mkStdGen 123123) [normalize $ rotateTile (defaultDeck !! 5) 0]
    --let board = boardFromDeck deck
    --grid <- displayBoard board 

    let game = sampleGame 

    gameBox <- displayState game

    containerAdd window gameBox

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
displayState game = do

    -- add the board
    vb <- vBoxNew False 10
    boardGrid <- displayBoard (board game)
    boxPackStart vb boardGrid PackNatural 0     

    -- add the current player's hand
    let currPlayer = currentPlayer game
    handBox <- displayHand (hand currPlayer)
    boxPackStart vb handBox PackGrow 10
    
    return vb

displayHand :: [Tile] -> IO HBox
displayHand hand = do
    hb <- hBoxNew True 10
    mapM_  (\t -> do
        img <- tileToImage (Just t)
        boxPackStart hb img PackNatural 0) hand
    return hb

displayBoard :: Board -> IO HBox
displayBoard b = do
    let coords = [(x,y) | y <- [0..5], x <- [0..5]]
    let tiles' = zip (concat $ tiles b) coords

    grid <- gridNew
    mapM_ (attachTile grid) tiles' -- maybe works?

    hb <- hBoxNew True 0
    boxPackStart hb grid PackNatural 0
    return hb

-- | Attaches a tile to a grid
attachTile :: Grid -> (Maybe Tile, Pos) -> IO ()
attachTile grid (tile, (x,y)) = do
    img <- tileToImage tile 
    gridAttach grid img x y 1 1    


tileToImage :: Maybe Tile -> IO Image
tileToImage Nothing = do
    fp <- tileToFilepath Nothing
    imageNewFromFile fp
tileToImage (Just tile) = do
    let rotations = rotationsFromBase tile          -- get rotations
    let rotBackTile = rotateTile tile (-rotations)  -- rotate backwards
    fp <- tileToFilepath (Just rotBackTile)         -- get base image
    img <- imageNewFromFile fp
    rotateImage img rotations                       -- rotate forward again

-- | Returns the number of 90 deg clockwise rotations from a base tile 
rotationsFromBase :: Tile -> Int    
rotationsFromBase t = rotationsFromBase' t 0

-- works by rotating counter clockwise until it is in the default deck
rotationsFromBase' tile num 
    | tile `elem` defaultDeck = num
    | otherwise = rotationsFromBase' (normalize (rotateTile tile 3)) (num + 1)

-- | Given an normalized [Connection] as string, returns the expected filepath
tileToFilepath :: Maybe Tile -> IO String
tileToFilepath tile = do
    root <- getCurrentDirectory
    let id = maybe "blank" (filter (/=',') . dropFirstAndLast . show . toList . normalize) tile
    return (root ++ "/assets/" ++ 
            id ++ ".png")

-- | Rotates an image by 90 deg n times clockwise
rotateImage :: Image -> Int -> IO Image
rotateImage img 0 = return img
rotateImage img n 
    | n < 0 = rotateImage img (4-n)
    | otherwise = do
        rotimg <- rotateImage' img
        rotateImage rotimg (n-1)

rotateImage' :: Image -> IO Image
rotateImage' img = do
    pb <- imageGetPixbuf img
    pbrot <- pixbufRotateSimple pb PixbufRotateClockwise
    imageNewFromPixbuf pbrot

dropFirstAndLast :: String -> String
dropFirstAndLast s = drop 1 $ take (-1 + length s) s