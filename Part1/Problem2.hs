module Problem2 where

fibb n =
  let
    fibbr n acc1 acc2 =
        case n of
        1 -> acc1 + acc2
        _ -> fibbr (n-1) (acc1 + acc2) acc1
  in
    fibbr n 1 0

result =
  let
    fibbList = [fibb n | n <- [1..]]
    smallerThan n = n < 4000000
    isEven n = (mod n 2) == 0
  in
    sum $ filter isEven $ takeWhile smallerThan fibbList
