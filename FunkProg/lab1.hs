-- The amount of steps of power n k will always be (n + 1) step
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power : negative argument"
power n 0 = 1
power n k = n * power n (k - 1)

-- listOfN is produced by list comprehension where the list size is determined
-- by k and the list content by n
power1 :: Int -> Int -> Int
power1 n k | k < 0 = error "power : negative argument"
power1 n 0 = 1
power1 n k = product listOfN
    where listOfN = take k [n,n..]

-- Usage of guard cases in order to run the right calculation depending on k
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power : negative argument"
power2 n 0 = 1
power2 n k
    | even k = (n * n)^(k `div` 2)
    | odd k = n * (n^(k - 1))
