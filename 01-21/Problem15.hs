module Problem15 where

{-
getRoutes len (x,y)
  | x == len && y == len = []
  | x == len = [ (x, y+1) ]
  | y == len = [ (x+1, y) ]
  | otherwise = [ (x, y+1), (x+1, y) ]

-- from (0,0) to (len, len)
getLatticePaths' len startPoints acc =
  case startPoints of
    [] -> acc
    xs -> getLatticePaths' len np (startPoints ++ acc)
  where
    np = concat $ map (getRoutes len) startPoints

getLatticePaths len = getLatticePaths' len [(0,0)] []
-}

-- using combinatorics
-- let right move is coded by 1, down by 0
-- we see that we have always 20 1s and 20 0s
-- in 20x20 grid to reach bottom right
-- so problem is: how to choose 20 items from 40 positions
-- answer: binominal formula
factorial n = foldl1 (*) [1..n]
result = (factorial 40) `div` ((factorial 20)^2)
