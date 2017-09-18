module Problem17 where

numberWord 1  = "one"
numberWord 2  = "two"
numberWord 3  = "three"
numberWord 4  = "four"
numberWord 5  = "five"
numberWord 6  = "six"
numberWord 7  = "seven"
numberWord 8  = "eight"
numberWord 9  = "nine"
numberWord 10 = "ten"
numberWord 11 = "eleven"
numberWord 12 = "twelve"
numberWord 13 = "thirteen"
numberWord 14 = "fourteen"
numberWord 15 = "fifteen"
numberWord 16 = "sixteen"
numberWord 17 = "seventeen"
numberWord 18 = "eighteen"
numberWord 19 = "nineteen"
numberWord 20 = "twenty"
numberWord 30 = "thirty"
numberWord 40 = "forty"
numberWord 50 = "fifty"
numberWord 60 = "sixty"
numberWord 70 = "seventy"
numberWord 80 = "eighty"
numberWord 90 = "ninety"
numberWord 1000 = "one thousand"

numberWord n
  | n < 100   = (numberWord (digit2 * 10)) ++ " " ++ (numberWord digit1)
  | otherwise = (numberWord digit3) ++ " hundred" ++ (getTenthsWord n)
    where
      digit3 = mod (div n 100) 10
      digit2 = mod (div n 10)  10
      digit1 = mod n 10

getTenthsWord n =
  case tenths of
    0 -> ""
    _ -> " and " ++ (numberWord tenths)
  where tenths = mod n 100

result =
  let removeWhitespaces = filter (\c -> c /= ' ')
  in sum $ map length [removeWhitespaces $ numberWord n | n <- [1..1000]]
