import Test.QuickCheck

-- The amount of steps of power will always be (n + 1) step
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power : negative argument"
power n 0 = 1
power n k = n * power n (k - 1)

-- listOfN is produced by list comprehension where the list size is determined
-- by k and the list content by n
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power : negative argument"
power1 n 0 = 1
power1 n k = product listOfN
    where listOfN = take (fromInteger k) [n,n..]

-- Usage of guard cases in order to run the right calculation depending if k
-- is odd or even
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power : negative argument"
power2 n 0 = 1
power2 n k
    | even k = (n * n)^(k `div` 2)
    | odd k = n * (n^(k - 1))

case1 = (2,2) -- Generic case
case2 = (2,-2) -- Negative argument should cause an error
case3 = (4.2, 8.9) -- Case to see if calculation can handle floats
case4 = (3.2312, -9.8) -- An negative case for floats

-- Checks if power1 and power2 gives the same answer as power
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k) == (power1 n k) && (power n k) == (power2 n k)

-- Negative power should not affect/cancel the testing process
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)

testCases :: (Integer, Integer) -> Bool
testCases (n, k) = prop_powers n k
