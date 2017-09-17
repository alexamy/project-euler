module Problem14 where

import Data.List

collatzSeq' list 1 = list ++ [1]
collatzSeq' list n =
  let
    isEven = (==) 0 $ mod n 2
    next =
      case isEven of
      True  -> div n 2
      False -> 3 * n + 1
  in
    collatzSeq' (list ++ [n]) next

collatzSeq = collatzSeq' []

collatzSeqLengths limit =
  [(i, length $ collatzSeq i) | i <- [1..limit-1]]

result =
  fst $ maximumBy maxFunc $ collatzSeqLengths 1000000
  where
    maxFunc (_, len1) (_, len2) = compare len1 len2
