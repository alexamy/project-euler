module Problem3 where

isPrime :: Integer -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n =
  not $ any (== True) $ map isFactor $ prevNumbers
  where
    prevNumbers = [2..(n-1)]
    isFactor = (==) 0 . mod n

primesList :: [Integer]
primesList =
  [i | i <- [1..], isPrime i]

primeFactors :: Integer -> Int -> [Integer]
primeFactors n pCount =
  [i | i <- primes, isFactor i]
  where
    isFactor i = (n `mod` i) == 0
    primes = take pCount primesList

result :: Integer
result =
  last $ primeFactors 600851475143 1000

-- check by
-- foldl (*) 1 result
