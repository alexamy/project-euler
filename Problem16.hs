module Problem16 where

import Data.Char

result = sum $ map digitToInt (show (2^1000))
