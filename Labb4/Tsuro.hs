module Tsuro where

import Data.Maybe

main = undefined


-- Magic constants for board size
bW = 6      -- board width
q = bW - 1  -- max board index

-- The entire game state, including players
data Game = Game {players :: Player, board :: Board, deck :: [Tile]}

-- The board, a list of rows of tiles, standard size is 6x6
data Board = Board {tiles :: [[Maybe Tile]], pieces :: [Piece]}

-- A player piece, situated at a tile and a connection
-- pieces do not have a position at "turn 0"
data Piece = Piece {iD :: ID, pos :: Maybe Pos, link :: Maybe Link}

-- A single tile, which
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

type Player = (ID, Hand)
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

-- Updates a piece (moves it forward until it reaches a bare connection)
-- * recursively
updatePiece :: Board -> Piece -> Board
updatePiece b p = if next_tile == Nothing
                  then Board (tiles b) (pieces b)
                  else updatePiece b new_piece
  where
    next_tile = nextTile b (fromJust (pos p)) (fromJust(link p))
    new_piece = Piece (iD p) (Just new_pos) new_link
    new_pos   = updatePos (fromJust (pos p)) (linkOffs (fromJust(link p)))
    new_link  = undefined -- TODO

updatePos :: Pos -> Pos -> Pos
updatePos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

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
                    (leftTiles ++ (Just new_tile) : rightTiles) :
                                    lowerRows
    where
            (upperRows, thisRow : lowerRows) = splitAt y ts
            (leftTiles, _ : rightTiles) = splitAt x thisRow

-- Given a position, returns all the "Manhattan"-adjacent tiles
-- * with respect to the edges of the board
adjacentPos :: Pos -> [Pos]
adjacentPos (a,b) = zip [  a,a+1,  a,a-1]
                        [b+1,  b,b-1,  b]

(+++) :: Pos -> Pos -> Pos
(+++) (a,b) (c,d) = (a+c,b+d)
