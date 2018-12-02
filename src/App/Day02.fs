module Day02

// problem page
// http://adventofcode.com/2018/day/2

open Utilities

let fileName = @"./data/02.txt"

let freqDic s =
    Seq.fold (
        fun (map: Map<_, int>) c ->
            if Map.containsKey c map then Map.add c ((Map.find c map) + 1) map
            else Map.add c 1 map)
            (Map.empty) s

let createFreqMaps (s : seq<string[]>) = 
    s
    |> Seq.map (fun a -> a.[0] |> Input.explode)
    |> Seq.map freqDic


let findNRepeats n m =
    m 
    |> Map.filter (fun _ v -> v = n)
    |> Map.count |> sign

let find2Repeats m = findNRepeats 2 m

let find3Repeats m = findNRepeats 3 m

let firstPuzzle() =
    let freqMap = Input.processFile fileName |> createFreqMaps
    let result = (freqMap |> Seq.map find2Repeats |> Seq.sum) * 
                 (freqMap |> Seq.map find3Repeats |> Seq.sum)
    printfn "%i" result     
