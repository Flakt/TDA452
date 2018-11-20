module Sudoku where
import Data.Maybe
import Test.QuickCheck

newtype Sudoku = Sudoku {rows :: [[Maybe Int]]}
    deriving (Show, Eq)

-- A1

-- | Returns a blank 9x9 sudoku grid
allBlankSudoku :: Sudoku
allBlankSudoku = 
    Sudoku (replicate 9 (replicate 9 Nothing))

allFilledSudoku :: Sudoku
allFilledSudoku = 
    Sudoku (replicate 9 (replicate 9 (Just 1)))

-- A2

-- | Tests if a sudoku confirms to the 9x9 dimensions
isSudoku :: Sudoku -> Bool
isSudoku sudoku = 
    and [length x == 9 | x <- rows sudoku] && length (rows sudoku) == 9

-- A3
isEmpty :: Sudoku -> Bool
isEmpty sudoku = 
    and [and [isNothing x | x <- row] | row <- rows sudoku]

isFilled :: Sudoku -> Bool
isFilled sudoku = 
    and [and [isJust x | x <- row] | row <- rows sudoku]


-- B1

printSudoku :: Sudoku -> IO ()
printSudoku s = print "todo" -- TODO

-- B2

readSudoku :: FilePath -> IO Sudoku
readSudoku fp = return allBlankSudoku -- TODO

--Here are some more functions that might come in handy:
--
--digitToInt :: Char -> Int
--putStr     :: String -> IO ()
--putStrLn   :: String -> IO ()
--readFile   :: FilePath -> IO String
--lines      :: String -> [String]
--unlines    :: [String] -> String

-- C1

-- | Generates a cell in (Just 1..9, Nothing)
cell :: Gen (Maybe Int)
cell = frequency [(1, return Nothing),
                  (9, do n <- choose (1,9)
                         return (Just n))]

-- C2

-- | Generates an arbitrary sudoku (with no strict checking)                        
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- C3

-- | 
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku