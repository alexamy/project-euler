module Problem10 where

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

result :: Integer -> Integer
result limit =
  sum $ takeWhile (\n -> n < limit) primesList
