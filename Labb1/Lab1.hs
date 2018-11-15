import Test.QuickCheck

-- The amount of steps of power will always be (k + 1) step

-- Basic recursive implementation
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power : negative argument"
power n 0 = 1
power n k = n * power n (k - 1)

-- Product on constructed list implementation
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power : negative argument"
power1 n k = product listOfN
    where listOfN = take (fromInteger k) [n,n..]

-- Improved (shortened) implementation
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power : negative argument"
power2 n 0 = 1
power2 n k
    | even k = power2 (n * n) (k `div` 2)
    | odd k = n * power2 n (k-1)

-- A
cases = [ (n,k) | n <- [-10,-9..10], k <- [0..10]]

-- B
-- Checks if power1 and power2 gives the same answer as power
prop_powers :: Integer -> Integer -> Bool
prop_powers n k =   power n k == power1 n k &&
                    power n k == power2 n k

-- Tests the power function with a 2-tuple of integers
testCase :: (Integer, Integer) -> Bool
testCase (n, k) = prop_powers n k

-- C
-- Tests all cases in a list containing 2-tuples of integers
testAllCases :: [(Integer, Integer)] -> Bool
testAllCases list = and [ testCase x | x <- list ]

-- D
-- Negative power should not affect/cancel the testing process
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)
