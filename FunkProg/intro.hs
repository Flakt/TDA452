import Test.QuickCheck

-- No specific type defintion of the functions, use :browse in GHCI for the
-- implicit type defintion

exchangeRate = 10.316

toEur sek = sek / exchangeRate

toSek eur = eur * exchangeRate

-- Test case
prop_exchange eur = toEur (toSek eur) ~== eur

-- Operator for testing when x - y reaches very small integers
x ~== y = abs(x-y) < 1e-5

-- Determing absolute values
absolute x | x<0    = -x
           | x >= 0 = x

-- Alternative implementation of the funciton above
absolute' x = if x<0 then -x else x

-- Recursive funciton as shown by power funciton
power n k | k < 0 = error "negative power"
power n k | k == 0 = 1
power n k = n * power n (k-1)

prop_power n k = power n k' == n^k'
    where k' = absolute k -- to avoid negative components
