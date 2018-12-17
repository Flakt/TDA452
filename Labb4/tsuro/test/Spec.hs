module Main where
import Tsuro

import Test.QuickCheck hiding (Result)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property

main :: IO ()
main = hspec $ describe "tsuro" $ do
--    prop "updateTile"   $ prop_updateTile
    prop "adjacentPos"  $ prop_adjacentPos


instance Arbitrary Tile where
     arbitrary =
        return (Tile dummyconns)

dummyconns = [(0,6),(1,5),(2,7),(3,6)]

--prop_updateTile :: [[Maybe Tile]] -> Pos ->  Tile -> Property
--prop_updateTile ts (x,y) t =
--    (length ts < y && length (ts !! y) < x) ==>
--    (new_tiles !! y) !! x ?== Just t
--    where new_tiles = updateTile ts (x,y) t

prop_adjacentPos :: Pos -> Result
prop_adjacentPos p = True ?== all f (adjacentPos p)
    where f x = x >-< p == 1

-- Manhattan distance
(>-<) :: Pos -> Pos -> Int
(>-<) (a,b) (c,d) = abs (a-c) + abs (b-d)