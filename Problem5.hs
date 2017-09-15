smallestDividibleByAll greatestDivider countTries =
  head $ flt (take countTries checkList) greatestDivider
  where
  flt list divider
    | list == []   = []
    | divider == 1 = list
    | otherwise =
        flt filteredList (divider-1)
        where
          filteredList = filter isDividible list
          isDividible i = (mod i divider) == 0
  checkList = [greatestDivider * i | i <- [1..]]

result =
  smallestDividibleByAll 20 100000000
