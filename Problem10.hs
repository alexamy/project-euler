module Problem10 where

import Problem3

result :: Integer -> Integer
result limit =
  sum $ takeWhile (\n -> n < limit) primesList
