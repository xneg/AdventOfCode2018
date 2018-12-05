module Day05

open Utilities;

let check a acc =
    let shouldDestroy (a : char) (b : char) = abs(int a - int b) = 32
    match acc with
    | [] -> [a]
    | [b] -> if shouldDestroy a b then [] else [a;b]
    | b::tail -> if shouldDestroy a b then tail else a::b::tail

let processData data =
    let rec recProcess acc data =
        match data with
        | [] -> acc
        | [a] -> check a acc
        | a::tail -> recProcess (check a acc) tail 
    recProcess [] data |> List.rev   

let fileName = @"./data/05.txt"

let data =
    fileName
    |> Input.readLines 
    |> Seq.concat
    |> Seq.toList

let firstPuzzle() =
    data
    |> processData
    |> List.length
    |> printfn "%i"

let removedChars =
    let removedCharsSmall = [ 'a' .. 'z' ]
    let removedCharsBig = [ 'A' .. 'Z' ]
    List.map2 (fun x y -> (x, y)) removedCharsSmall removedCharsBig

let removeChar (a, b) data =
    data |> List.filter (fun x -> x <> a && x <> b)

let secondPuzzle() =
    [for c in removedChars -> 
        data |> removeChar c |> processData |> List.length] 
    |> List.min
    |> printfn "%i"    
