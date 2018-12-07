module Day07

open System.Text.RegularExpressions
open System.IO
open Day04


let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine line =
    match line with
    | Regex @"Step (.) must be finished before step (.) can begin." [ parent; child ] -> printfn "parent %s child %s" parent child
    | _ -> failwith "Wrong input data"


let readLines (filePath: string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line
        }

let fileName = @"./data/07.txt"

let data = 
    fileName 
    |> readLines
    |> Seq.map (parseLine)
