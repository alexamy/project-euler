module Problem1 where

result =
  let isDividible n = (mod n 3) == 0 || (mod n 5) == 0 in
  sum $ filter isDividible [1..999]
