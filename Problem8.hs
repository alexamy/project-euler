module Problem8 where

import Data.Char
import DataValues (numberString_pr8)

numberString = DataValues.numberString_pr8
substrings = sublists

sublists list trunkLen =
  sublists' list trunkLen []
  where
  sublists' list trunkLen acc
    | trunkLen > length list = acc
    | otherwise =
      sublists' (tail list) trunkLen accum
      where
      accum = acc ++ [take trunkLen list]

result =
  maximum $ map productOfDigits digitLists
  where
  productOfDigits = foldl1 (*)
  digitLists = map digitize subs
  digitize = map digitToInt
  subs = substrings numberString 13
