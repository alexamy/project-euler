module Problem12 where

isFactor n f = mod n f == 0

triangleNumber n =
  sum [1..n]

divisors n =
  [ f | f <- [1 .. n], isFactor n f]

-- as faster as number of factors, 1x for primes
-- only half of list, without (div n f)
divisorsOpt :: Int -> [Int] -> Int -> Int -> [Int]
divisorsOpt n divList greatDiv f
  | f >= greatDiv = divList
  | otherwise =
    case isFactor n f of
      True  -> divisorsOpt n (divList ++ [f]) (div n f) (f + 1)
      False -> divisorsOpt n divList          greatDiv  (f + 1)

divisors' n = divisorsOpt n [] n 1

-- result 1000000
result limit =
  head $ filter winCondition triangleNumbers
  where
    triangleNumbers = [triangleNumber i | i <- [1 .. limit]]
    winCondition n = (2 * (length $ divisors' n)) > 500
