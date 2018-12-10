module Sudoku where
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.Environment
import           Test.QuickCheck

newtype Sudoku = Sudoku {rows :: [[Maybe Int]]}
    deriving (Eq)

instance Show Sudoku where
    show = sudokuToString

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- A1

-- | Returns a blank 9x9 sudoku grid

allBlankSudoku :: Sudoku
allBlankSudoku =
    Sudoku (replicate 9 (replicate 9 Nothing))

-- A2

-- | Tests if a sudoku conforms to the 9x9 dimensions and that all values are in [1..9]
isSudoku :: Sudoku -> Bool
isSudoku sudoku =
    all ((==9) . length) (rows sudoku) &&
    all (`elem` legalVals) (concat (rows sudoku))
    where
        legalVals = Nothing:[Just n | n <- [1..9]]

-- A3   

-- | Tests if a sudoku is filled, i.e. contains no Nothing values
isFilled :: Sudoku -> Bool
isFilled sudoku =
    all isJust (concat (rows sudoku))

-- B1

-- | Prints a given sudoku
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr (sudokuToString s)

sudokuToString :: Sudoku -> String
sudokuToString s = concatMap rowToString (rows s)

rowToString :: [Maybe Int] -> String
rowToString row = concatMap cellToString row ++ "\n"

cellToString :: Maybe Int -> String
cellToString (Just n) = show n
cellToString Nothing  = "."

-- B2

-- | Given a filepath, returns a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
    s <- readFile fp
    let sud = sudokuFromString s
    if isSudoku sud
    then return sud
    else error "readSudoku: bad file"

-- | Returns a sudoku given a string ('\n' as line separator, '.' as empty)
sudokuFromString :: String -> Sudoku
sudokuFromString string =
    Sudoku (map stringToRow (lines string))
    where
        stringToRow = map toMaybe

toMaybe :: Char -> Maybe Int
toMaybe '.' = Nothing
toMaybe c   = Just ((fromEnum c :: Int) + (-48))


-- C1

-- | Generates a cell in (Just 1..9, Nothing)
cell :: Gen (Maybe Int)
cell = frequency [(3, return Nothing),
                  (1, do n <- choose (1,9)
                         return (Just n))]

-- C2

-- | Generates an arbitrary sudoku (with no strict checking)
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- C3

-- | Tests the size constraints
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-- D1

type Block = [Maybe Int]

-- | tests a block (9 elements) such that there are no duplicate numbers
isOkayBlock :: Block -> Bool
isOkayBlock block =
    nub numbers == numbers
    where
        numbers = [fromMaybe 0 x | x <- block, isJust x ] :: [Int]

-- D2
-- | Given a sudoku, converts it into all different blocks,
-- i.e. rows, columns and squares
blocks :: Sudoku -> [Block]
blocks s = rows s ++ transpose (rows s) ++ allSquares s
    where
        allSquares s = concatMap appendRows (chunksOf 3 (rows s))

-- | Given 3 rows, concatenates them into 3 blocks
appendRows :: [[Maybe Int]] -> [[Maybe Int]]
appendRows r
    | length r == 3 = [b1, b2, b3]
        where
        b1 = concatMap (take 3) r
        b2 = concatMap (take 3 . drop 3) r
        b3 = concatMap (drop 6) r
appendRows _ = error "appendRows: bad input"

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s =
    all (==9) (map length (blocks s))

-- D3

-- | Tests a sudoku such that a number does not occurr twice in a block
-- (i.e. rows, columns and 3x3 subsquares)
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

-- E1

type Pos = (Int, Int)

-- | Given a sudoku, returns a list of coordinates
-- corresponding to empty spaces
blanks :: Sudoku -> [Pos]
blanks s =
        map fst         -- only return the point
        (filter snd     -- remove non-Nothing
        (zip coords (   -- combine with a point
            concatMap (map isNothing) (rows s)))
        )
        where
            coords = [(x,y) | y <- [0..8], x<-[0..8]]


prop_blanks :: Sudoku -> Bool
prop_blanks sud =
    all isNothing (map (valueAt sud) results)
    where results = blanks sud

valueAt :: Sudoku -> Pos -> Maybe Int
valueAt sud (x,y) =
    (rows sud !! y) !! x

-- E2

-- | Given a list, and a tuple (index, new_value), updates the element in
-- the list at the given index to the given new_value
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) [] (_, _) = error "!!= : applied to empty list"
(!!=) x (n, _) | n < 0 || n > length x
    = error "!!= : index out of bounds"
(!!=) (x:xs) (0, nv) = nv:xs
(!!=) (x:xs) (n, nv) = x:(xs !!= (n-1,nv))

-- | Tests that the expected value can be found at the updated position
prop_bangBangEquals_correct :: Eq a => [a] -> (Int, a) -> Bool
prop_bangBangEquals_correct list (bi, nv) =
    null list || -- auto ok illegal argument
    newList !! i == nv
    where
        newList = list !!= (i, nv)
        i = bi `mod` length list

-- E3

-- | Updates a given sudoku given a position and a new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (x,y) nv =
    Sudoku [ if row == y then list !!= (x, nv) else list
             | (row, list) <- zip [0..8] (rows s)]

-- | Tests that the expected value can be found at the updated cell
prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_updated s (bx,by) nv =
    (rows newSudoku !! y) !! x == nv
    where
        newSudoku = update s (x,y) nv
        x = bx `mod` 9
        y = by `mod` 9

-- E4

-- | Given a sudoku and a position, returns a list of candidates
-- (i.e. legalVals numbers) that could be placed at the position
candidates :: Sudoku -> Pos -> [Int]
candidates s (x,y) =
    [fromJust c | c <- [Just n | n <- [1..9]], c `notElem` relevantCells]
    where
        relevantCells = rows !! y ++
                        cols !! x ++
                        squares !! (x `div` 3 + (y `div` 3) * 3)
        [rows, cols, squares] = chunksOf 9 (blocks s)

-- | Given a sudoku, tests if prop_candidates_correct_cell holds for all cells
prop_candidates_correct :: Sudoku -> Bool
prop_candidates_correct s =
    all (prop_candidates_correct_cell s) coords
    where coords = [(x,y) | x <- [0..8], y <- [0..8]]

-- | Given and a sudoku, tests if placing all candidates at that cell results
-- in a legalVals sudoku (i.e. isSudoku && isOkay)
prop_candidates_correct_cell :: Sudoku -> Pos -> Bool
prop_candidates_correct_cell s (bx,by) =
    (bx,by) `elem` blanks s ||          -- ok illegal arguments
    not (isOkay s)          ||
    (all isSudoku possibleSudokus &&
    all isOkay possibleSudokus)
    where
        x = bx `mod` 9
        y = by `mod` 9
        possibleSudokus = map (update s (x,y) . Just) (candidates s (x,y))

-- F1

-- | Given a Sudoku, returns a solution or Nothing if there are none
solve :: Sudoku -> Maybe Sudoku
solve sud | isSudoku sud && isOkay sud = solve'' sud
          | otherwise                  = Nothing

-- | Attempts to solve a given sudoku by trying all the candidates of a
-- given position.
solve' :: Sudoku -> Pos -> Maybe Sudoku
solve' sud pos
    | null cands = Nothing
    | otherwise = listToMaybe (catMaybes solutions)
    where
        solutions   = map solve'' children
        children    = map (update sud pos . Just) cands -- list of all possible children
        cands       = candidates sud pos

-- | Solves a sudoku
solve'' :: Sudoku -> Maybe Sudoku
solve'' sud   | null blanks' = Just sud
              | otherwise    = solve' sud (head blanks')
                where blanks' = blanks sud

-- F2

-- | See solve
readAndSolve :: FilePath -> IO ()
readAndSolve fp = do
    sud <- readSudoku fp
    let solution = solve sud
    maybe (print "(no solution)") print solution

-- F3

-- | Tests if the first sudoku is a solution of the second sudoku
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol sud =
    isOkay     sol &&     -- all blocks are legalVals
    isSudoku   sol &&     -- the dimensions are correct
    isFilled   sol &&     -- there are no blanks
    isSubsetOf sol sud

-- | Tests that for all pairs of corresponding values in sol and sud
-- that sol.v == sud.v OR that sud.v == Nothing
isSubsetOf :: Sudoku -> Sudoku -> Bool
isSubsetOf sol sud =
    all (\(a,b) -> a == b || isNothing b) zipped
    where
        zipped = zip (toList sol) (toList sud)
        toList = concat . rows

-- F4

-- | Tests that all solutions are sound
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud =
    isSudoku sud && isOkay sud ==> solution `isSolutionOf` sud
    where
        (Just solution) = solve sud
