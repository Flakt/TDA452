module Main where
import Tsuro
import Test.QuickCheck

main :: IO ()
main = print "not implemented"
    
instance Arbitrary Tile where
    arbitrary = undefined -- TODO   

prop_updateTile :: [[Maybe Tile]] -> Pos -> Tile -> Bool
prop_updateTile ts (x,y) t =
    (new_tiles !! y) !! x == Just t
    where new_tiles = updateTiles ts (x,y) t

prop_adjacentPos :: Pos -> Bool
prop_adjacentPos p = all f (adjacentPos p)
    where f x = x >-< p == 1

-- Manhattan distance
(>-<) :: Pos -> Pos -> Int
(>-<) (a,b) (c,d) = abs (a-c) + abs (b-d)