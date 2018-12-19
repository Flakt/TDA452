module Tsuro where
import Data.List
import Data.List.Split
import Data.Maybe
import System.Random

-- TODO evaluate if a Map is more useful in some cases

-- Magic constants for board size
bw = 6      -- board width
q = bw - 1  -- max board index

-- | The entire game state, including players
data Game = Game {players :: [Player], board :: Board, deck :: [Tile], currPlayer :: Player}

gameNew :: StdGen -> [PiecePos] -> (Game, StdGen)
gameNew gen startPos = (Game players' boardNew deck' (head players'), gen')
    where
        players' = playersNew startPos
        (deck',gen') = shuffle gen deck  
        deck = deckNew

-- | Places the tile in the current position (i.e. in front of the active player)
-- returning a new state which has updated
--      - the board (added the new tile)
--      - the current player's hand (removed played tile and drawn new)
--      - the deck (drawn from)
gameMakeMove :: Game -> StdGen -> Tile -> (Game, StdGen)
gameMakeMove game gen tile = (game {players = players', board = board', 
                                    currPlayer = nextPlayer},           gen')
    where
        gen'        = undefined
        players'    = undefined
        board'      = updateBoard b nextPos tile
        nextPlayer  = undefined

        nextPos = pos +++ linkOffs link
        (pos, link) = movePlayer b (start p)   -- TODO get the next tile instead
        
        b = board game

        p' = p {hand = hand'}
            where hand' = hand p \\ [tile]
        p = currPlayer game

sampleGame :: Game
sampleGame = Game ps (boardFromDeck rem) [] (head ps)
    where
        ps = [Player 0 h ((0,0),0)]
        (h,rem) = drawNTiles 2 deckNew

-- | Construct a tile from a list of links
tile' :: [Link] -> Tile
tile' [a,b,c,d,e,f,g,h] = Tile [(a,b),(c,d),(e,f),(g,h)]

toList :: Tile -> [Link]
toList (Tile [(a,b),(c,d),(e,f),(g,h)]) = [a,b,c,d,e,f,g,h]

-- | The board, a list of rows of tiles, standard size is 6x6
newtype Board = Board {tiles :: [[Maybe Tile]]}
boardNew = Board (replicate bw (replicate bw Nothing))
instance Show Board where
    show b = concat lines
        where 
            lines = map ((++ "\n") . f) (tiles b)
            f = concatMap (maybe "." (const "T"))

-- | Fills a board with a list of tiles, from top left to bottom right
boardFromDeck :: [Tile] -> Board
boardFromDeck deck = Board $ chunksOf 6 ts'
    where
        ts' = ts ++ replicate (36 - length ts) Nothing -- adds blanks to end
        ts = map Just deck              
        
-- Updates the board by placing the tile on the given position
updateBoard :: Board -> Pos -> Tile -> Board
updateBoard b p t = Board (updateTile (tiles b) p t)

-- | A single tile, which is represented by a list of connections
newtype Tile = Tile {conn :: [Connection]}
    deriving (Eq, Show, Read)

-- | Returns a new tile
tileNew :: StdGen -> (Tile, StdGen)
tileNew gen = (tile' ls, gen') 
    where
        (ls, gen') = shuffle gen [0..7]
        
-- | Returns a new deck of tiles (always the same order)
deckNew :: [Tile]
deckNew = defaultDeck

-- | Shuffles a list
shuffle :: Eq a => StdGen -> [a] -> ([a], StdGen)
shuffle gen ls = shuffle' gen ls []

shuffle' :: Eq a => StdGen -> [a] -> [a] -> ([a], StdGen)
shuffle' gen  [] new = (new, gen)
shuffle' gen old new = shuffle' gen' (delete toAdd old) (toAdd : new)
    where
        toAdd = old !! v    
        (v, gen') = randomR (0, length old + (-1)) gen

-- | Draws n cards from a deck, returning the drewn card and the remainder
drawNTiles :: Int -> [Tile] -> ([Tile], [Tile])  
drawNTiles = splitAt

{-  Connections are internal within the Tile.
    Below is a representation of how the Links are indexed
    Rotating a tile is done by transposing its connections

     01
    7XX2
    6XX3
     54
-}
type Connection = (Link, Link)
type Link = Int

-- | A player, represented by an id, a hand (which is a list of tiles)
-- and a starting position. The starting position can be used to derive
-- the current position using movePlayer
data Player = Player {playerId :: Int, hand :: [Tile], start :: PiecePos}
                      deriving (Show, Eq)

type PiecePos = (Pos, Link)

-- | Given a list of starting positions, returns a list of new players
playersNew :: [PiecePos] -> [Player]
playersNew []     = []
playersNew (x:xs) = playersNew xs ++ [Player n [] x]
    where n = length xs

-- | Checks if a player is dead on a given board state
playerIsGameOver :: Board -> Player -> Bool
playerIsGameOver b p = undefined -- TODO @fan fix this shit yo

type Hand = [Tile]

type Pos = (Int, Int)

-- | Simulates the movement of a player piece (moves it forward until it 
-- reaches a bare connection or collides with an another player) and returns 
-- the final position, tile and connection
movePlayer :: Board -> PiecePos -> PiecePos
movePlayer b (pos, exitLink) 
    | isOutside newPos  = (pos, exitLink)   -- cant move to next, because it is OOB
    | isNothing newTile = (pos, exitLink)   -- cant move to next because it is empty
    | otherwise         =  movePlayer b (newPos, newExitLink)
    where
        newExitLink  = findOtherLink newEntryLink (fromJust newTile)
        newEntryLink = mapLinks exitLink
        newTile      = b @@ newPos
        newPos       = pos +++ linkOffs exitLink

-- | Returns the other link connected to the link in the tile
findOtherLink :: Link -> Tile -> Link
findOtherLink l t = other l $ head $ filter (\ (a, b) -> a == l || b == l) (conn t)

-- | Given a value and a tuple containing that value, 
-- returns the other value in the tuple
other :: Eq a => a -> (a,a) -> a
other x (a,b) | x == a    = b
              | x == b    = a
              | otherwise = error "other : x is neither"

-- | Returns the connection in the given tile containing the given link
findConnection :: Link -> Tile -> Connection
findConnection l t = fromJust(find f (conn t))
    where f (a, b) = a == target || b == target
          target   = mapLinks l

-- | Checks if a given position is out of the game boundaries
isOutside :: Pos -> Bool
isOutside pos = (x > bw || y > bw) || (x < 0 || y < 0)
    where
      x = fst pos
      y = snd pos

-- | Maps a link with the link it leads to when moving from a tile
mapLinks :: Link -> Link
mapLinks l | even l = (l + 5) `mod` 8
mapLinks l | odd  l = (l + 3) `mod` 8
 
-- | Returns Just the next tile to travel to, or Nothing
nextTile :: Board -> Pos -> Link -> Maybe Tile
nextTile b p l = b @@ p'
    where
        p' = p +++ linkOffs l

-- | Returns the tile, or Nothing
-- Will throw index errors if given bad pos
(@@) :: Board -> Pos -> Maybe Tile
(@@) b (x,y) = (tiles b !! y) !! x

-- | Returns the offset in position from a given link
linkOffs :: Link -> (Int, Int)
linkOffs l  | l == 0 || l == 1 = ( 0,-1)
            | l == 2 || l == 3 = ( 1, 0)
            | l == 4 || l == 5 = ( 0, 1)
            | l == 6 || l == 7 = (-1, 0)
            | otherwise = error "linkOffs : bad value"

-- Replaces a tile in a matrix of tiles with a given tile
updateTile :: [[Maybe Tile]] -> Pos -> Tile -> [[Maybe Tile]]
updateTile ts (x,y) new_tile
    | x < 0 || x > x_max || y < 0 || y > y_max = error $ 
                        "updateTiles : pos " ++ show (x,y) ++ "out of bounds"
    | otherwise =                   upperRows ++
                    (leftTiles ++ Just new_tile : rightTiles) :
                                    lowerRows
    where
            (upperRows, thisRow : lowerRows) = splitAt y ts
            (leftTiles, _ : rightTiles) = splitAt x thisRow
            x_max = length (ts !! y)
            y_max = length ts

-- Given a position, returns all the "Manhattan"-adjacent tiles
-- * with no respect to the edges of the board
adjacentPos :: Pos -> [Pos]
adjacentPos (a,b) = zip [  a,a+1,  a,a-1]
                        [b+1,  b,b-1,  b]

-- | Position addition
(+++) :: Pos -> Pos -> Pos
(+++) (a,b) (c,d) = (a+c,b+d)

-- | Rotates a tile (by rotating its connections) by 90 deg clockwise n times
-- negative values result in a counter clockwise rotation   
rotateTile :: Tile -> Int -> Tile
rotateTile t n  
    | n < 0     = rotateTile t (4+n)
    | otherwise = Tile (map transposeConn (conn t))
    where transposeConn (a,b) = (transposeLink a,transposeLink b)
          transposeLink x = (x +(2 * n)) `mod` 8

-- | Normalizes a tile such that all connections have their lowest link first
-- and that the list of connections is sorted on the first link in each conn
normalize :: Tile -> Tile
normalize t = Tile newConn
    where 
        newConn = sortOn fst $
                  map (\(a,b) -> if a < b then (a,b) else (b,a)) (conn t)

defaultDeck :: [Tile]
defaultDeck =     -- 1
    [ tile' [0,1,2,3,4,5,6,7] -- ok 
    , tile' [0,7,1,4,2,5,3,6] -- ok
    , tile' [0,6,1,7,2,5,3,4] -- ok
    , tile' [0,2,1,3,4,5,6,7] -- ok
    , tile' [0,4,1,6,2,5,3,7] -- ok
    , tile' [0,6,1,5,2,3,4,7] -- ok
    -- 2
    , tile' [0,4,1,2,3,7,5,6] -- ok
    , tile' [0,5,1,4,2,7,3,6] -- ok
    , tile' [0,7,1,6,2,3,4,5] -- ok
    , tile' [0,7,1,3,2,4,5,6] -- ok
    , tile' [0,2,1,5,3,4,6,7] -- ok
    , tile' [0,3,1,4,2,6,5,7] -- ok
    -- 3
    , tile' [0,1,2,7,3,5,4,6] -- ok 
    , tile' [0,4,1,7,2,6,3,5] -- ok
    , tile' [0,2,1,4,3,5,6,7] -- ok
    , tile' [0,2,1,7,3,5,4,6] -- ok
    , tile' [0,4,1,5,2,3,6,7] -- ok
    , tile' [0,2,1,3,4,6,5,7] -- ok
    -- 4
    , tile' [0,4,1,5,2,6,3,7] -- ok
    , tile' [0,1,2,5,3,6,4,7] -- ok
    , tile' [0,6,1,5,2,4,3,7] -- ok
    , tile' [0,1,2,6,3,5,4,7] -- ok
    , tile' [0,3,1,6,2,5,4,7] -- ok
    , tile' [0,1,2,7,3,6,4,5] -- ok
    -- 5
    , tile' [0,5,1,7,2,4,3,6] -- ok 
    , tile' [0,7,1,2,3,4,5,6] -- ok
    , tile' [0,1,2,7,3,4,5,6] -- ok
    , tile' [0,3,1,2,4,7,5,6] -- ok
    , tile' [0,3,1,7,2,5,4,6] -- ok
    , tile' [0,7,1,3,2,6,4,5] -- ok
    -- 6
    , tile' [0,6,1,5,2,7,3,4] -- ok 
    , tile' [0,5,1,6,2,4,3,7] -- ok
    , tile' [0,2,1,6,3,4,5,7] -- ok
    , tile' [0,5,1,4,2,6,3,7] -- ok
    , tile' [0,4,1,3,2,7,5,6] -- ok
    ]