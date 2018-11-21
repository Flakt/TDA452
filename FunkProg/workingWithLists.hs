import Prelude hiding ((++),null,reverse,take,drop,splitAt,zip,unzip)
import qualified Prelude
import Test.QuickCheck

-- Lecture is most liekly about recursion and use of pattern matching

-- List type defined
data List a = Nil | Cons a (List a)

-- List are built using the commands x:xs and []

-- Some operators for list
null :: [a] -> Bool
null []    = True
null _     = False

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

cons x xs = x:xs

snoc :: [a] -> a -> [a]
snoc xs x = xs++[x]

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- Take the first n elements of a listOfN
take :: Int -> [a] -> [a]
take 0 xs = []
take n [] = []
take n (x:xs) = x : take (n - 1) xs

-- Test for function Take
prop_take n xs = n >= 0 ==> length (take n xs) == min n (length xs)

-- Discard the first n elements of a list
drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = []
drop n []          = []
drop n (_:xs)      = drop (n - 1) xs

-- Test for function Drop
prop_take_drop :: Int -> [Int] -> Bool
prop_take_drop n xs = take n xs ++ drop n xs == xs

nonprop_take_drop :: Int -> [Int] -> Bool
nonprop_take_drop n xs = drop n xs ++ take n xs == xs

-- Combine a pair of list into a list of pairs
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _      _      = []

-- Split a list of pairs into a pair of lists
unzip :: [(a,b)] -> ([a],[b])
unzip xys = ([x | (x,y) <- xys], [y | (x,y) <- xys])

-- Alternative implementation
unzip' :: [(a,b)] -> ([a],[b])
unzip' ((x,y) : xys) = (x:xs, y:ys)
  where (xs, ys) = unzip' xys

-- Quicksort in Haskell
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
  where
    smaller = [x' | x' <- xs, x' < x]
    bigger  = [x' | x' <- xs, x' > x]

-- | insert a new element at the right position in a sorted list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (x':xs) | x<=x' = x:x':xs
                     | otherwise = x':insert x xs

isSorted [] = True
isSorted [x] = True
isSorted (x1:x2:xs) = x1<=x2 && isSorted (x2:xs)

prop_insert :: Int -> [Int] -> Property
prop_insert x xs = isSorted xs ==> isSorted (insert x xs)
-- QuickCheck said "Gave up! Passed only 73 tests.". We will return to this.
-- in a later lecture.

-- | Insertion sort (sort a list by using insert)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

prop_qsort :: [Int] -> Bool
prop_qsort xs = qsort xs == isort xs
