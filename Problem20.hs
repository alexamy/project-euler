module Problem20 where

result =
  sum digits
  where
    digits = map charToInt digitsString
    charToInt c = (read [c])::Int
    digitsString = show (factorial 100)
    factorial n = foldl1 (*) [1..n]
