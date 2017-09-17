module Problem12 where

triangleNumber n =
  sum [1..n]

divisors n =
  [ f | f <- [1..n], isFactor f]
  where
    isFactor = (==) 0 . mod n

result limit =
  head $ filter (\n -> (length $ divisors n) > 500) triangleNumbers
  where
    triangleNumbers = [triangleNumber i | i <- [1..limit]]
