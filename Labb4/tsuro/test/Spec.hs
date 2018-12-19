module Main where
import Tsuro

import Data.List

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Gen
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property
import System.Random

main :: IO ()
main = hspec $ describe "tsuro" $ do
--    prop "updateTile"   $ prop_updateTile
    prop "tileNew : connections"    prop_tileNew_conn
    prop "adjacentPos : distance"   prop_adjacentPos_distance
    prop "adjacentPos : uniqueness" prop_adjacentPos_uniqueness
    prop "mapLinks"                 prop_mapLinks
    prop "normalize"                prop_normalize
    prop "deckNew : normalized"     prop_deckNew_normalized
    prop "deckNew : uniqeness"      prop_deckNew_uniqueness
    prop "shuffle"                  prop_shuffle
    prop "rotateTile : identity"    prop_rotateTile_identity
    prop "rotateTile : reverse"     prop_rotateTile_reverse

instance Arbitrary Tile where
     arbitrary = do
        n <- arbitrarySizedNatural
        let gen = mkStdGen n
        return (fst (tileNew gen))

tile :: Gen Tile
tile = arbitrary

--------------------------------------------------------------

-- tests that 4 rotations results in the original tile
prop_rotateTile_identity :: Tile -> Result
prop_rotateTile_identity t = rotateTile t 4 ?== t

-- tests that rotating forward then backward results in the original tile
prop_rotateTile_reverse :: Tile -> Int -> Result
prop_rotateTile_reverse t n = rotateTile (rotateTile t n) (-n) ?== t

prop_shuffle :: [Int] -> Int -> Result
prop_shuffle list seed = list ==? (list `intersect` list')
        where (list',_) = Tsuro.shuffle (mkStdGen seed) list

prop_deckNew_normalized :: Result
prop_deckNew_normalized = True ==? all (checkNormalize . conn) deckNew

prop_deckNew_uniqueness :: Result
prop_deckNew_uniqueness = [] ==? (deckNew \\ nub deckNew)

prop_normalize :: Tile -> Result
prop_normalize t = True ==? checkNormalize (conn t') 
    where t' = normalize t
    
checkNormalize :: [Connection] -> Bool
checkNormalize [] = True
checkNormalize ((a,b):(c,d):xs) = checkNormalize xs && a < b && c < d && a < c 

--prop_updateTile :: [[Maybe Tile]] -> Pos ->  Tile -> Property
--prop_updateTile ts (x,y) t =
--    (length ts < y && length (ts !! y) < x) ==>
--    (new_tiles !! y) !! x ?== Just t
--    where new_tiles = updateTile ts (x,y) t

-- tests that the connections include all numbers 0 to 7 exactly once
prop_tileNew_conn :: Tile -> Result
prop_tileNew_conn t = fromConns ?== [0..7]
    where 
        fromConns = sort $ foldr f [] (conn t) 
        f (a,b) ls = a:b:ls

-- tests that the function returns correctly
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

-- tests that all return positions actually are adjacent
prop_adjacentPos_distance :: Pos -> Result
prop_adjacentPos_distance p = True ?== all f (adjacentPos p)
    where f x = x >-< p == 1

-- tests that there are no duplicate positions
prop_adjacentPos_uniqueness :: Pos -> Result
prop_adjacentPos_uniqueness pos = [] ==? (res \\ nub res) 
    where res = adjacentPos pos
    
-- Manhattan distance
(>-<) :: Pos -> Pos -> Int
(>-<) (a,b) (c,d) = abs (a-c) + abs (b-d)