import Data.List

isPalindrome :: Int -> Bool
isPalindrome n =
   str == (reverse str)
  where
    str = show n

result :: Int
result =
  maximum productList

productList :: [Int]
productList = [mult | i <- [1..999],
                      j <- [1..999],
                      let mult = i*j,
                      isPalindrome mult]
