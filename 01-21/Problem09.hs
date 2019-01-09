module Problem9 where

import Data.Fixed

pythNumbers lim =
  [ resList | a <- [1 .. lim],
              b <- [1 .. lim],
              a < b,
              let c = sqrt $ a^2 + b^2,
              let isSuit i = (==) 0 $ mod' i 1,
              let resList = map truncate [a,b,c],
              isSuit c]

result limit =
  let numberLists = pythNumbers limit in
  (product . concat . take 1) $ filter ((==) 1000 . sum) numberLists
