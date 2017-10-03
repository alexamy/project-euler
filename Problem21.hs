module Problem21 where

divisors :: Integral a => a -> [a]
divisors n = [d | d <- [1..(n-1)], (mod n d) == 0]

isAmicable :: Integral a => a -> Bool
isAmicable n = (n == sum2) && (n /= sum1)
  where
    sum1 = sum (divisors n)
    sum2 = sum (divisors sum1)

amicableList :: Integral a => a -> [a]
amicableList limit =
  [n | n <- [1..limit], isAmicable n]

result = sum $ amicableList 10000
