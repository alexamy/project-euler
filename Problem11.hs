module Problem11 where

import DataValues (numberGrid_pr10)

numberGrid = numberGrid_pr10

type Matrix = [[Integer]]
-- find greatest product of 4 adjacent numbers (up, down, left, right, vertical)
-- maybe refactoring in future - create matrix data type

sampleGrid =
  [ [01,02,03,04],
    [05,06,07,08],
    [09,10,11,12],
    [13,14,15,16] ]

--index from 1
elemAt :: [Integer] -> Int -> Integer
elemAt list pos
  | pos < 1   = 0
  | pos > length list = 0
  | otherwise = list !! (pos-1)

-- index from (1, 1)
elemAtMatrix :: Matrix -> (Int, Int) -> Integer
elemAtMatrix matrix (row, col)
  | row < 1 = 0
  | row > length matrix = 0
  | otherwise =
      let rowList = matrix !! (row-1) in
      elemAt rowList col

adjacentElements :: Matrix -> (Int, Int) -> (Int, Int) -> Int -> [Integer]
adjacentElements matrix (elemRow, elemCol) (incrRow, incrCol) count =
  [elemAtMatrix matrix (r, c) | i <- [0 .. count-1],
                                let r = elemRow + i * incrRow,
                                let c = elemCol + i * incrCol]

productOfAdj :: Matrix -> (Int, Int) -> Int -> [Integer]
productOfAdj matrix (incrRow, incrCol) count =
  [product $ adj  | row <- [1 .. rowsNumber],
                    col <- [1 .. colsNumber],
                    let adj = adjacentElements matrix (row, col) (incrRow, incrCol) count]
  where
    rowsNumber = length $ matrix
    colsNumber = length $ matrix !! 0

result :: Integer
result =
  maximum $ concat $
    [ productOfAdj numberGrid (0, 1) 4,
      productOfAdj numberGrid (1, 0) 4,
      productOfAdj numberGrid (1, 1) 4,
      productOfAdj numberGrid ((-1), 1) 4]
