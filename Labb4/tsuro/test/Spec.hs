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
    prop "mapLinks"     $ prop_mapLinks


instance Arbitrary Tile where
     arbitrary =
        return (Tile dummyconns)

--instance Arbitrary Tile when
--        arbitrary = 
--            list <- shuffle [0..7]


        
dummyconns = [(0,6),(1,5),(2,7),(3,6)]

--prop_updateTile :: [[Maybe Tile]] -> Pos ->  Tile -> Property
--prop_updateTile ts (x,y) t =
--    (length ts < y && length (ts !! y) < x) ==>
--    (new_tiles !! y) !! x ?== Just t
--    where new_tiles = updateTile ts (x,y) t

prop_mapLinks :: Int -> Result
prop_mapLinks 0 = mapLinks 0 ?== 5
prop_mapLinks 1 = mapLinks 1 ?== 4
prop_mapLinks 2 = mapLinks 2 ?== 7
prop_mapLinks 3 = mapLinks 3 ?== 6
prop_mapLinks 4 = mapLinks 4 ?== 1
prop_mapLinks 5 = mapLinks 5 ?== 0
prop_mapLinks 6 = mapLinks 6 ?== 3
prop_mapLinks 7 = mapLinks 7 ?== 2
prop_mapLinks n = prop_mapLinks (abs n `mod` 8)

prop_adjacentPos :: Pos -> Result
prop_adjacentPos p = True ?== all f (adjacentPos p)
    where f x = x >-< p == 1

-- Manhattan distance
(>-<) :: Pos -> Pos -> Int
(>-<) (a,b) (c,d) = abs (a-c) + abs (b-d)