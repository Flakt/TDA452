module Tsuro where
import Data.List
import Data.Maybe

-- TODO evaluate if a Map is more useful in some cases

-- Magic constants for board size
bw = 6      -- board width
q = bw - 1  -- max board index

-- | The entire game state, including players
data Game = Game {players :: Player, board :: Board, deck :: [Tile]}

-- | The board, a list of rows of tiles, standard size is 6x6
data Board = Board {tiles :: [[Maybe Tile]]}
emptyBoard = Board (replicate bw (replicate bw Nothing))
instance Show Board where
    show b = concat lines
        where 
            lines = map (++"\n") $ map f (tiles b)
            f = concatMap (maybe "." (\x -> "T"))

-- | A single tile, which
newtype Tile = Tile {conn :: [Connection]}
    deriving (Eq, Show)
    
{-  Connections are internal within the Tile
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
-- and a starting position
data Player = Player {playerId :: ID, hand :: [Tile], pos :: Pos,
                      link :: Link}
type ID = Int

-- A list of tiles (strictly speaking it is unordered)
type Hand = [Tile]

type Pos = (Int, Int)

-- Updates the board by placing the tile on the given position
-- * causes pieces to slide
updateBoard :: Board -> Pos -> Tile -> Board
updateBoard b p t = Board (updateTile (tiles b) p t)

-- | Simulates the movement of a player piece (moves it forward until it 
-- reaches a bare connection or collides with an another player) and returns 
-- the final position, tile and connection
movePlayer :: Board -> Pos -> Link -> (Pos, Maybe Tile, Connection)
movePlayer b p l | isOutside newPos = (newPos, tile, connection)
                 | isNothing tile   = (newPos, tile, connection)
                 | otherwise        =  movePlayer b newPos newLink
    where
      tile       = b @@ p
      connection = findConnection l (fromJust tile)
      newPos     = p +++ linkOffs l
      newLink    = mapLinks l

-- | Returns the connection in the given tile containing the given link
findConnection :: Link -> Tile -> Connection
findConnection l t = fromJust(find f (conn t))
    where f (a, b) = a == target || b == target
          target   = mapLinks l

-- | Checks if a given pos is out of the game boundaries
isOutside :: Pos -> Bool
isOutside pos = (x > bw || y > bw) || (x < 0 || y < 0)
    where
      x = fst pos
      y = snd pos

-- | Maps links with corresponding link of the other tile
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
(@@) b (x,y) = (tiles b !! x) !! y

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
    | x < 0 || x > x_max || y < y_max || y > q = error "updateTiles : pos out of bounds"
    | otherwise =                   upperRows ++
                    (leftTiles ++ Just new_tile : rightTiles) :
                                    lowerRows
    where
            (upperRows, thisRow : lowerRows) = splitAt y ts
            (leftTiles, _ : rightTiles) = splitAt x thisRow
            x_max = length (ts !! y)
            y_max = length ts

-- Given a position, returns all the "Manhattan"-adjacent tiles
-- * with respect to the edges of the board
adjacentPos :: Pos -> [Pos]
adjacentPos (a,b) = zip [  a,a+1,  a,a-1]
                        [b+1,  b,b-1,  b]

-- | The legal positions on a given board for a given player
-- returns Nothing if the player doesn't have a piece
-- TODO should maybe consider that the position is on the board
legalPos :: Board -> Player -> Maybe Pos
legalPos b p = undefined -- TODO

-- | Position addition
(+++) :: Pos -> Pos -> Pos
(+++) (a,b) (c,d) = (a+c,b+d)

-- | Rotates a tile (by rotating its connections) by 90 deg
rotateTile :: Tile -> Tile
rotateTile t = Tile (map transposeConn (conn t))
    where transposeConn (a,b) = (transposeLink a,transposeLink b)
          transposeLink x = (x +2) `mod` 8

--detectCollision :: Board -> Player -> Player -> Bool
--detectCollision b p1 p2 = samePosAndLink
--    where samePosAndLink = pos p1 && pos p2 && link p1 && link p2
