module Day01

// problem page
// http://adventofcode.com/2017/day/1

open Utilities

let fileName = @"./data/01.txt"

let seqNumbers =
    Input.processFile fileName
    |> Seq.map (fun a -> a.[0])
    |> Seq.map int

let firstPuzzle() = 
    seqNumbers 
    |> Seq.sum
    |> printfn "%i"

let findFirstTwiceFreq input =
    let cycle seq =
        let inner (list: list<int>) listLength n = list.[n % listLength]  
        let list = seq |> Seq.toList
        list |> List.length |> inner list    

    let rec inner hash currFreq n f =
        let currFreq = currFreq + f(n)
        if Set.contains currFreq hash then currFreq
        else
            let hash = hash |> Set.add currFreq
            inner hash currFreq (n + 1) f

    cycle input |> inner (Set.ofSeq [0]) 0 0        

let secondPuzzle() =
    findFirstTwiceFreq seqNumbers
    |> printfn "%i"