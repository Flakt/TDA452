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
import Control.Arrow

main :: IO ()
main = do
    void initGUI
    window <- windowNew
    set window [ windowTitle         := "Tsuro"
               , windowResizable     := True]
    
    vbox <- vBoxNew False 10
    buttonBox <- hBoxNew True 10

    gen <- getStdGen
    let (game, _) = gameNew gen 4
    
    st <- newIORef game

    gameBox <- displayState vbox window st game

    containerAdd vbox buttonBox
    containerAdd vbox gameBox

    btnR <- mkButton vbox window st (gameRotateHand 1)
    btnL <- mkButton vbox window st (gameRotateHand (-1))
    set btnR [buttonLabel := "Rotate Right"]
    set btnL [buttonLabel := "Rotate Left"]

    cd <- getCurrentDirectory
    testImage <- imageNewFromFile (cd ++ "/assets/dragon.png")
    testImage2 <- imageNewFromFile (cd ++ "/assets/player0.png")
    overlayImage testImage2 testImage

    -- test box
    evtBox <- eventBoxNew
    containerAdd evtBox testImage
    evtBox `on` buttonPressEvent $ tryEvent $
        liftIO $ putStrLn "static label clicked"

    containerAdd buttonBox btnR
    containerAdd buttonBox btnL
    containerAdd buttonBox evtBox

    containerAdd window vbox

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    widgetShowAll window    
    mainGUI

-- | Makes a button tile
mkTileButton :: VBox -> Window -> IORef Game -> (Game -> Game) -> Tile -> IO EventBox
mkTileButton b w st f t = do
    evtBox <- eventBoxNew
    img <- renderTile (Just t)
    evtBox `containerAdd` img
    evtBox `on` buttonPressEvent $ tryEvent $
        liftIO $ modifyState b w st f
    return evtBox

-- | Makes a button that changes the state and updates the view
mkButton :: VBox -> Window ->IORef Game -> (Game -> Game)  -> IO Button
mkButton box window st f = do
    button <- buttonNew
    button `on` buttonActivated $ modifyState box window st f
    return button

-- | 
modifyState :: VBox -> Window -> IORef Game -> (Game -> Game) -> IO ()
modifyState b w st f = do
        modifyIORef st f
        
        newState <- readIORef st
        gameBox <- displayState b w st newState

        children <- containerGetChildren b   -- get children
        containerRemove b (last children)    -- remove first child (hopefully the top)
        containerAdd b gameBox               -- add new render
        
        print $ show newState

        widgetShowAll w

-- | Creates a new element based on a game state, containing 
--   - a grid of images
--   - the current player's hand
--   - buttons to rotate or place
displayState :: VBox -> Window -> IORef Game -> Game -> IO VBox
displayState b w st game = do

    -- add the board
    vb <- vBoxNew False 10
    boardGrid <- displayBoard (board game) (players game)
    boxPackStart vb boardGrid PackNatural 0     

    -- add the current player's hand
    let current = getCurrentPlayer game
    let ls = hand (getCurrentPlayer game)
    handBox <- displayHand b w st ls
    widgetModifyBg handBox StateNormal (playerColor (playerId current))
    boxPackStart vb handBox PackGrow 10 
    
    return vb

-- | Renders a hand as a row of tiles
displayHand :: VBox -> Window -> IORef Game -> [Tile] -> IO HBox
displayHand b w st hand = do
    hb <- hBoxNew True 10
    mapM_  (\t -> do
        img <- mkTileButton b w st (gameMakeMove t) t
        boxPackStart hb img PackNatural 0) hand
    return hb

-- | Renders a board as a grid of tiles with player pieces
displayBoard :: Board -> [Player] -> IO HBox
displayBoard b ps = do
    let coords = [(x,y) | y <- [0..5], x <- [0..5]]
    let tiles' = zip (concat $ tiles b) coords

    let playerPositions = map (\x -> (movePlayer b (start x),  playerId x)) ps -- could be done with &&&

    grid <- gridNew
    gridSetRowHomogeneous grid True
    gridSetColumnHomogeneous grid True
    mapM_ (attachTile grid playerPositions) tiles'

    hb <- hBoxNew True 0
    boxPackStart hb grid PackNatural 0
    return hb

-- | overlays image a ontop of b
overlayImage :: Image -> Image -> IO Image
overlayImage a b = do
    bufA <- imageGetPixbuf a
    bufB <- imageGetPixbuf b
    pixbufComposite bufA bufB 0 0 80 80 0 0 1 1 InterpNearest 255
    imageNewFromPixbuf bufB

-- | Attaches a overlay with an image to the grid
attachTile :: Grid -> [(PiecePos, Int)] -> (Maybe Tile, Pos) -> IO ()
attachTile grid ls (tile, (x,y)) = do
    img <- renderTile tile

    let ls' = filter filterToTile ls
    let border = filter filterBorder ls -- list of pieces on the outside border

    mapM_ (\(( (px,py) ,l) ,id) -> do
        pImg <- renderPiece id l 
        overlayImage pImg img) ls'
    
    -- mapM_ (\(( (px,py) ,l) ,id) -> do
    --     pImg <- renderPiece id (mapGates l) 
    --     overlayImage pImg img) ls'
    
    gridAttach grid img x y 1 1   
    where
        filterToTile (((px,py),_),_) = px == x && py == y 
        filterBorder (((px,py),_),_) = (x == 0 && px == x-1 && py == y) || 
                                       (x == bi && px == x+1 && py == y) ||
                                       (y == 0 && px == x && py == y+1) ||
                                       (y == bi && px == x && py == y-1) 

-- | Renders a player's piece
renderPiece :: Int -> Gate -> IO Image -- TODO this can probably be improved
renderPiece id l  = do
        root    <- getCurrentDirectory
        img     <- imageNewFromFile $ root ++ "/assets/player" ++ show id ++ ".png" 
        imgFlip <- fliphImage img 
        rotateImage (if odd l then imgFlip else img) (l `div` 2)

-- | Renders a tile
renderTile :: Maybe Tile -> IO Image
renderTile Nothing = do
    fp <- tileToFilepath Nothing
    imageNewFromFile fp
renderTile (Just tile) = do
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
    let id = maybe "blank" (filter (/=',') . concatMap show . toList . normalize) tile
    return (root ++ "/assets/" ++ 
            id ++ ".png")

-- | Flips an image horizontally (i.e. over x-axis)
fliphImage :: Image -> IO Image
fliphImage img = do
    pb <- imageGetPixbuf img
    pbflip <- pixbufFlipHorizontally pb
    imageNewFromPixbuf pbflip

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


playerColor :: Int -> Color
playerColor 0 = Color 40000 0 0
playerColor 1 = Color 0 40000 0
playerColor 2 = Color 40000 40000 5000
playerColor 3 = Color 0 0 40000
playerColor 4 = Color 40000 0 40000
playerColor 5 = Color 0 0 65000
playerColor 6 = Color 40000 20000 20000
playerColor 7 = Color 0 40000 20000