module Problem3 where

-- number X cant have prime factor greater or equal sqrt(X)
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n =
  not $ any (== True) $ map isFactor $ prevNumbers
  where
    upLimit = (ceiling . sqrt . fromIntegral) n
    prevNumbers = [2 .. upLimit]
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
