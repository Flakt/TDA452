module Tsuro where
import Data.List
import Data.List.Split
import Data.Maybe
import System.Random

-- Magic constants for board size
bw = 6      -- board width
bi = bw - 1  -- max board index

-- | The entire game state, including players
data Game = Game {players :: [Player],
                  board :: Board, 
                  deck :: [Tile], 
                  turnNum :: Int}
                        deriving (Show, Eq)

-- | A player, represented by an id, a hand (which is a list of tiles)
-- and a starting position. The starting position can be used to derive
-- the current position using movePlayer
data Player = Player {playerId :: Int, 
                      hand :: [Tile], 
                      start :: PiecePos}
                        deriving (Show, Eq)

-- | The board, a list of rows of tiles, standard size is 6x6
newtype Board = Board {tiles :: [[Maybe Tile]]}
                        deriving (Eq)

-- | A single tile, which is represented by a list of connections
newtype Tile = Tile {conn :: [Connection]}
                        deriving (Eq, Show, Read)

{-  Connections are internal within the Tile.
    Below is a representation of how the Gates are indexed
    Rotating a tile is done by transposing its connections

     01
    7XX2
    6XX3
     54
-}
type Connection = (Gate, Gate)
type Gate = Int
type PiecePos = (Pos, Gate)
type Hand = [Tile]
type Pos = (Int, Int)

-- | Generates a new game using a std gen and number of players (max. 8)
gameNew :: StdGen -> Int -> (Game, StdGen)
gameNew gen n
    | n > 8 = error "gameNew : too many players"
    | otherwise = (Game players' boardNew deck' 0, gen'')
    where
        (players',deck') = foldr drawForAll ([], deck) players 
        
        players = playersNew positions
        (positions,gen'') = randomEdgePositions gen' n
        (deck,gen') = shuffle gen deckNew

-- helper for fold
drawForAll :: Player -> ([Player], [Tile]) -> ([Player],[Tile])
drawForAll plr (ls, d) = (plr{hand = drawn} : ls, rem)
    where (drawn, rem) = drawNTiles 3 d

-- | Rotates the current player's hand counterwise or clockwise
gameRotateHand :: Int -> Game -> Game
gameRotateHand n g = g{players = players'}
    where
        players' = map (\x -> if x == current then current' else x) (players g)
        current' = current{hand = map (`rotateTile` n) (hand current)}
        current = getCurrentPlayer g

-- | Gets the current player via ID
getCurrentPlayer :: Game -> Player
getCurrentPlayer g = players g !! getCurrentPlayerID g

-- | Generates n random unique starting points
randomEdgePositions :: StdGen -> Int -> ([PiecePos],StdGen)
randomEdgePositions gen n = if allDifferent res     -- test uniqueness
                            then (res,gen') 
                            else randomEdgePositions gen' n
    where (res,gen') = randomEdgePositions' gen n

randomEdgePositions' :: StdGen -> Int -> ([PiecePos],StdGen)
randomEdgePositions' gen 0 = ([],gen)
randomEdgePositions' gen n = (pos : childVal,childGen)
    where 
        (childVal, childGen)     = randomEdgePositions' gen' (n-1)
        (pos, gen') = randomEdgePos gen

-- | Generates a random starting point
randomEdgePos :: StdGen -> (PiecePos, StdGen)
randomEdgePos gen = (pos, gen')
    where
        pos = numToStartPos n
        (n, gen') = randomR (0,bw*8 - 1) gen

-- | Magical function that converts an integer to a PiecePos
numToStartPos :: Int -> PiecePos
numToStartPos v 
    | v `elem` [bw*0 .. bw*2-1] = ((    (v - bw*0) `div` 2, -1), 4 + v `mod` 2)
    | v `elem` [bw*2 .. bw*4-1] = ((bw, (v - bw*2) `div` 2    ), 6 + v `mod` 2)
    | v `elem` [bw*4 .. bw*6-1] = ((    (v - bw*4) `div` 2, bw), 0 + v `mod` 2)
    | v `elem` [bw*6 .. bw*8-1] = ((-1, (v - bw*6) `div` 2    ), 2 + v `mod` 2)

-- | Places the tile in the current position (i.e. in front of the active player)
-- returning a new state which has updated
--      - the board (added the new tile)
--      - the current player's hand (removed played tile and drawn new)
--      - the deck (drawn from)
gameMakeMove :: Tile -> Game -> Game
gameMakeMove tile game = 
    game{players = players', board = board', turnNum = turnNum game + 1}
    where
        players'    = map (\x -> if x == p then p' else x) (players game)
        board'      = updateBoard b nextPos tile
        -- nextPlayer  = getNextPlayer b p (players game)

        nextPos = pos +++ gateOffs gate         -- find pos to place tile
        (pos, gate) = movePlayer b (start p)    -- simulate the player
        b = board game

        p' = p {hand = hand'}
            where hand' = (hand p \\ [tile]) ++ drawnCard
        (drawnCard, rem) = drawNTiles 1 (deck game) 
        p = getCurrentPlayer game

-- | Gets the ID of the current player (that is not game over)
getCurrentPlayerID :: Game -> Int
getCurrentPlayerID g = getCurrentPlayerID' g 0

-- | Recursive function for determining the current players ID
getCurrentPlayerID' :: Game -> Int -> Int
getCurrentPlayerID' g n | not isDead = pIdx
                        | otherwise  = getCurrentPlayerID' g (n+1) 
    where
        pIdx   = (turnNum g + n) `mod` length (players g)
        isDead = playerIsGameOver (board g) (players g !! pIdx)
        
-- | Construct a tile from a list of Gates
tile' :: [Gate] -> Tile
tile' [a,b,c,d,e,f,g,h] = Tile [(a,b),(c,d),(e,f),(g,h)]

-- | Breaks down a tile into a list of Gates
toList :: Tile -> [Gate]
toList (Tile [(a,b),(c,d),(e,f),(g,h)]) = [a,b,c,d,e,f,g,h]

instance Show Board where
    show b = concat lines
        where
            lines = map ((++ "\n") . f) (tiles b)
            f = concatMap (maybe "." (const "T"))

boardNew = Board (replicate bw (replicate bw Nothing))

-- Updates the board by placing the tile on the given position
updateBoard :: Board -> Pos -> Tile -> Board
updateBoard b p t = Board (updateTile (tiles b) p t)

-- | Returns a new tile
tileNew :: StdGen -> (Tile, StdGen)
tileNew gen = (tile' ls, gen')
    where (ls, gen') = shuffle gen [0..7]

-- | Returns a new deck of tiles (always the same order)
deckNew :: [Tile]
deckNew = defaultDeck

-- | Shuffles a list
-- stolen from our own blackjack project
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

-- | Given a list of starting positions, returns a list of new players
playersNew :: [PiecePos] -> [Player]
playersNew []     = []
playersNew (x:xs) = playersNew xs ++ [Player (length xs) [] x]

-- | Checks if a player is dead on a given board state
-- tests this by seeing if the player reaches an edge that isnt starting pos
playerIsGameOver :: Board -> Player -> Bool
playerIsGameOver b p = onEdge piecePoint && piecePoint /= start p
        where piecePoint = movePlayer b (start p) 

onEdge :: PiecePos -> Bool
onEdge ((x,y), l) =
    (x == 0 && (l == 6 || l == 7)) || -- on left edge
    (x == bi && (l == 2 || l == 3)) || -- on right edge
    (y == 0 && (l == 0 || l == 1)) || -- on upper edge
    (y == bi && (l == 4 || l == 5))    -- on bottom edge

-- | Simulates the movement of a player piece (moves it forward until it
-- reaches a bare connection or collides with an another player) and returns
-- the final position, tile and connection
movePlayer :: Board -> PiecePos -> PiecePos
movePlayer b (pos, exitGate) 
    | isOutside newPos  = (pos, exitGate)   -- cant move to next, because it is OOB
    | isNothing newTile = (pos, exitGate)   -- cant move to next because it is empty
    | otherwise         =  movePlayer b (newPos, newExitGate)
    where
        newExitGate  = findOtherGate newEntryGate (fromJust newTile)
        newEntryGate = mapGates exitGate
        newTile      = b @@ newPos
        newPos       = pos +++ gateOffs exitGate

-- | Returns thef other Gate connected to the Gate in the tile
findOtherGate :: Gate -> Tile -> Gate
findOtherGate l t = other l $ head $ filter (\ (a, b) -> a == l || b == l) (conn t)

-- | Given a value and a tuple containing that value,
-- returns the other value in the tuple
other :: Eq a => a -> (a,a) -> a
other x (a,b) | x == a    = b
              | x == b    = a
              | otherwise = error "other : x is neither"

-- | Returns the connection in the given tile containing the given Gate
findConnection :: Gate -> Tile -> Connection
findConnection l t = fromJust(find f (conn t))
    where f (a, b) = a == target || b == target
          target   = mapGates l

-- | Checks if a given position is out of the game boundaries
isOutside :: Pos -> Bool
isOutside pos = (x > bw || y > bw) || (x < 0 || y < 0)
    where
      x = fst pos
      y = snd pos

-- | Maps a Gate with the Gate it leads to when moving from a tile
mapGates :: Gate -> Gate
mapGates l | even l = (l + 5) `mod` 8
mapGates l | odd  l = (l + 3) `mod` 8

-- | Returns Just the next tile to travel to, or Nothing
nextTile :: Board -> Pos -> Gate -> Maybe Tile
nextTile b p l = b @@ (p +++ gateOffs l)

-- | Returns the tile, or Nothing
-- Will throw index errors if given bad pos
(@@) :: Board -> Pos -> Maybe Tile
(@@) b (x,y) = (tiles b !! y) !! x

-- | Returns the offset in position from a given Gate
gateOffs :: Gate -> (Int, Int)
gateOffs l  | l == 0 || l == 1 = ( 0,-1)
            | l == 2 || l == 3 = ( 1, 0)
            | l == 4 || l == 5 = ( 0, 1)
            | l == 6 || l == 7 = (-1, 0)
            | otherwise = error "GateOffs : bad value"

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
adjacentPos p = map (+++ p) [(0,1),(1,0),(-1,0),(0,-1)]

-- | Position addition
(+++) :: Pos -> Pos -> Pos
(+++) (a,b) (c,d) = (a+c,b+d)

-- | Rotates a tile (by rotating its connections) by 90 deg clockwise n times
-- negative values result in a counter clockwise rotation
rotateTile :: Tile -> Int -> Tile
rotateTile t n
    | n < 0     = rotateTile t (4+n)
    | otherwise = Tile (map transposeConn (conn t))
    where transposeConn (a,b) = (transposeGate a,transposeGate b)
          transposeGate x = (x +(2 * n)) `mod` 8

-- | Returns true if all elements in the given list are unique
-- stolen from stackoverflow
allDifferent :: (Eq a) => [a] -> Bool
allDifferent list = case list of
    []      -> True
    (x:xs)  -> x `notElem` xs && allDifferent xs

-- | Normalizes a tile such that all connections have their lowest Gate first
-- and that the list of connections is sorted on the first Gate in each conn
-- used to easily map to a image file in IO
normalize :: Tile -> Tile
normalize t = Tile $ sortOn fst $
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
