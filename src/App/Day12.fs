module Day12

open System.Text.RegularExpressions
open System.Collections.Generic
open System.IO

let readLines (filePath: string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line
        }

let fileName = @"./data/12.txt"

let input = fileName |> readLines 

let rules = new Dictionary<string, string>()

let parse str =
    let x = Regex.Split(str, @"(^[\.#]{5}) => ([\.#])")
    x.[1], x.[2]

input |> Seq.iter (fun x -> 
    let key, value = parse x 
    rules.Add(key, value))

let (|Match|) input = rules.[input]

let initial = "##.####..####...#.####..##.#..##..#####.##.#..#...#.###.###....####.###...##..#...##.#.#...##.##.."

let grow data =
    match data with
    | Match r -> printfn "%s" r

grow "...##"

