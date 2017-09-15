module Problem2

(*
let fibb n = 
    | 1 -> 1
    | 2 -> 2
    | _ -> fibb (n-1) + fibb (n-2)
*)

let fibb n =
    let rec fibbr n acc1 acc2 =
        match n with
        | 1 -> acc1 + acc2
        | _ -> fibbr (n-1) (acc1 + acc2) acc1
    fibbr n 1 0

let solve =
    Seq.initInfinite (fun n -> fibb n)
    |> Seq.takeWhile (fun n -> n < 4000000)
    |> Seq.filter (fun n -> (n % 2) = 0)
    |> Seq.sum