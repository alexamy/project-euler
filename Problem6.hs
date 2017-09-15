squareOfSum n =
  sum [i*j | i <- [1..n], j <- [1..n]]

sumOfSquares n =
  sum [i^2 | i <- [1..n]]

result n =
  (squareOfSum n) - (sumOfSquares n)

resultOptimized n =
  sum [i*j | i <- [1..n], j <- [1..n], i /= j]
