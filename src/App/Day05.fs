module Day05

open Utilities;

let check a acc =
    let shouldDestroy a b = abs(int a - int b) = 32
    match acc with
    | [] -> [a]
    | b::tail -> if shouldDestroy a b then tail else a::b::tail

let processData data =
    let rec recProcess acc data =
        match data with
        | [] -> acc
        | a::tail -> recProcess (check a acc) tail 
    recProcess [] data |> List.length  

let removedChars = List.map2 (fun x y -> (x, y)) [ 'a' .. 'z' ] [ 'A' .. 'Z' ]

let removeChar (a, b) = List.filter (fun x -> x <> a && x <> b)

let fileName = @"./data/05.txt"

let data =
    fileName
    |> Input.readLines 
    |> Seq.concat
    |> Seq.toList

let firstPuzzle() =
    data
    |> processData
    |> printfn "%i"

let secondPuzzle() =
    [for c in removedChars -> 
        data |> removeChar c |> processData] 
    |> List.min
    |> printfn "%i"    
