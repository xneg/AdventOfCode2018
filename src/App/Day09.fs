module Day09

let ccw _ (L, R)  =
    match R with 
    | [] -> 
          let head::tail = (L |> List.rev)
          ([head], tail)
    | a::tail -> (a::L, tail)   

let iterateSelf func initial count =
    let rec inner intermediate n =
        if n = count then
            (func n) intermediate
        else
            inner ((func n) intermediate) (n + 1)
    inner initial 0

let playersCount = 459 
let mutable playersScore = [|for i in 0 .. playersCount - 1 -> 0L|]

let put =
    let rec inner x (L, R) =
        if x = 0 || x % 23 <> 0 then
            match L with
            | [] when List.isEmpty R -> ([x], R)
            | [] -> inner x (List.rev R, [])
            | a::tail -> (tail, x::a::R)
        else
            let a::b::tail, R = iterateSelf ccw (L, R) 7
            let player = x % playersCount
            playersScore.[player] <- playersScore.[player] + int64(x + a) 
            (tail, b::R)
    inner    

let initial = ([], [])

let firstPuzzle() =
    playersScore <- [|for i in 0 .. playersCount - 1 -> 0L|]
    iterateSelf put ([], []) 72103 
    playersScore |> Array.max |> printfn "%i"

let secondPuzzle() =
    playersScore <- [|for i in 0 .. playersCount - 1 -> 0L|]
    iterateSelf put initial 7210300 
    playersScore |> Array.max |> printfn "%i"