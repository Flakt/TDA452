module Tsuro where
import Data.List
import Data.Maybe

-- TODO evaluate if a Map is more useful in some cases

-- Magic constants for board size
bW = 6      -- board width
q = bW - 1  -- max board index

-- | The entire game state, including players
data Game = Game {players :: Player, board :: Board, deck :: [Tile]}

-- | The board, a list of rows of tiles, standard size is 6x6
data Board = Board {tiles :: [[Maybe Tile]], pieces :: [Piece]}

-- | A single tile, which
newtype Tile = Tile {conn :: [Connection]}
    deriving (Eq)

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

data Player = Player {playerId :: ID, hand :: [Tile], pos :: Maybe Pos,
                      link :: Maybe Link}
type ID = Int

-- A list of tiles (strictly speaking it is unordered)
type Hand = [Tile]

type Pos = (Int, Int)

-- Updates the board
-- * causes pieces to slide
updateBoard :: Board -> Pos -> Tile -> Board
updateBoard b p t = Board new_tiles new_players
    where
        new_players = undefined -- TODO
        new_tiles = updateTiles (tiles b) p t

-- | Simulates the movement of a player piece (moves it forward until it reaches
--  a bare connection or collides with an another player) * recursively
movePlayer :: Board -> Player -> (Tile, Connection)
movePlayer

-- Returns Just the next tile to travel to, or Nothing
nextTile :: Board -> Pos -> Link -> Maybe Tile
nextTile b p l = b @@ p'
    where
        p' = p +++ linkOffs l

-- "board @@ position" returns the tile (if any) at the position
(@@) :: Board -> Pos -> Maybe Tile
(@@) b (x,y) = (tiles b !! x) !! y

-- Returns the offset in position from a given link
linkOffs :: Link -> (Int, Int)
linkOffs l  | l == 0 || l == 1 = ( 0,-1)
            | l == 2 || l == 3 = ( 1, 0)
            | l == 4 || l == 5 = ( 0, 1)
            | l == 6 || l == 7 = (-1, 0)
            | otherwise = error "linkOffs : bad value"

-- Updates a given tile
updateTiles :: [[Maybe Tile]] -> Pos -> Tile -> [[Maybe Tile]]
updateTiles ts (x,y) new_tile
    | x < 0 || x > q || y < 0 || y > q = error "updateTiles : pos out of bounds"
    | otherwise =                   upperRows ++
                    (leftTiles ++ Just new_tile : rightTiles) :
                                    lowerRows
    where
            (upperRows, thisRow : lowerRows) = splitAt y ts
            (leftTiles, _ : rightTiles) = splitAt x thisRow

-- Given a position, returns all the "Manhattan"-adjacent tiles
-- * with respect to the edges of the board
adjacentPos :: Pos -> [Pos]
adjacentPos (a,b) = zip [  a,a+1,  a,a-1]
                        [b+1,  b,b-1,  b]

-- | The legal positions on a given board for a given player
-- returns Nothing if the player doesn't have a piece
-- TODO should maybe consider that the position is on the board
legalPos :: Board -> Player -> Maybe Pos
legalPos b p = if isJust piece then Just pos' else Nothing
    where
        pos' = fromJust (pos piece') +++ linkOffs  (fromJust (link piece'))
        piece' = fromJust piece
        piece = find (\x -> pieceId x == playerId p) (pieces b)

-- | Position addition
(+++) :: Pos -> Pos -> Pos
(+++) (a,b) (c,d) = (a+c,b+d)

-- | Rotates a tile (by rotating its connections) by 90 deg
rotateTile :: Tile -> Tile
rotateTile t = Tile (map transposeConn (conn t))
    where transposeConn (a,b) = (transposeLink a,transposeLink b)
          transposeLink x = (x +2) `mod` 8

detectCollision :: Board -> Player -> Player -> Bool
detectCollision b p1 p2 = samePosAndLink
    where samePosAndLink = pos p1 && pos p2 && link p1 && link p2
