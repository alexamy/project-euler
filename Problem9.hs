module Problem9 where

import Data.Fixed

pythNumbers lim =
  [ map truncate [i, j, pythNum]
    | i <- [1 .. lim],
      j <- [1 .. lim],
      i < j,
      let pythNum = eqSolve i j,
      isSuit pythNum]
  where
    eqSolve a b = sqrt $ a^2 + b^2
    isSuit a = (mod' a 1) == 0.0

result limit =
  getProduct $ head $ filter filterF $ zipWith zipF pNumLists pNumSums
  where
    getProduct (lst, _) = foldl1 (*) lst
    filterF = (\(_, sum) -> sum == 1000)
    zipF pnums sum = (pnums, sum)
    pNumSums = map sum pNumLists
    pNumLists = pythNumbers limit
