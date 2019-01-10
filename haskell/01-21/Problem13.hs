module Problem13 where

import Problem13Data (numbers)

result =
  (take 10 . show . sum) numbers
