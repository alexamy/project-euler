module Problem18 where

import Problem18Data (numberTriangle, numberTriangle2)

part :: Int -> Int -> [a] -> [a]
part start len xs = take len $ drop start xs

subtriangle' :: [[a]] -> Int -> Int -> [[a]] -> [[a]]
subtriangle' []         start len acc = acc
subtriangle' (row:rest) start len acc =
  let currPart = part start len row
  in  subtriangle' rest start (len + 1) (acc ++ [currPart])

subtriangle :: Int -> Int -> [[a]] -> [[a]]
subtriangle depth start triangle =
  let latterTriangle = drop depth triangle
  in  subtriangle' latterTriangle start 1 []

findMaximumSumPath' :: [[Integer]] -> Integer -> Integer
findMaximumSumPath' [] acc = acc
findMaximumSumPath' triangle acc =
  let sumElement = (triangle !! 0) !! 0
      triangleLeft  = subtriangle 1 0 triangle
      triangleRight = subtriangle 1 1 triangle
      sum' = sum . concat
      triangleNext  = if (sum' triangleLeft) > (sum' triangleRight)
                      then triangleLeft else triangleRight
  in  findMaximumSumPath' triangleNext (acc + sumElement)

findMaximumSumPath :: [[Integer]] -> Integer
findMaximumSumPath triangle =
  findMaximumSumPath' triangle 0

sumAllSubtriangles :: [[Integer]] -> [[Integer]]
sumAllSubtriangles triangle =
  let depth = length triangle
  in [[ (sum . concat) $ subtriangle d r triangle | r <- [0..d] ]  | d <- [0..depth-1]]

result :: Integer
result =
  findMaximumSumPath numberTriangle

triangleTuples :: [[Integer]] -> [[(Integer, Integer)]]
triangleTuples triangle =
  let len = length triangle
      sums = sumAllSubtriangles triangle
  in  [ zip (triangle !! r) (sums !! r) | r <- [0..len-1] ]
