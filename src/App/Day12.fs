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

let parse str =
    let x = Regex.Split(str, @"(^[\.#]{5}) => ([\.#])")
    x.[1], x.[2]

let addLeftRight s = "....." + s + "....."

let fileName = @"./data/12.txt"

let input = fileName |> readLines 

let initial = "##.####..####...#.####..##.#..##..#####.##.#..#...#.###.###....####.###...##..#...##.#.#...##.##.."

let firstPuzzle() =

    let rules = new Dictionary<string, string>()

    input |> Seq.iter (fun x -> 
        let key, value = parse x 
        rules.Add(key, value))

    let (|Match|_|) (input: string) = 
        if input.Length < 5 then None 
        else
            let rule = input.[0..4]
            if rules.ContainsKey(rule) then
                Some(rules.[rule])
            else 
                Some(".")        

    let grow data =
        let data = addLeftRight data
        let rec inner acc data =
            match data with
            | Match r -> inner (r::acc) data.[1..]
            | _ -> acc |> List.rev |> List.fold (+) ""
        let result = inner [] data
        let remove = result.IndexOf("#")  
        result.[remove..result.LastIndexOf("#")], remove - 3 

    let growSteps initial (n: int64) =
        let rec inner i (n: int64) acc data =
            if (i = n) then data, acc
            else
                let data, newAcc = grow data
                inner (i + 1L) n (acc + newAcc) data
        inner 0L n 0 initial        

    let result, offset = growSteps initial 50000000000L
    [for c in result -> c] |> List.mapi (fun i c -> if c = '#' then i + offset else 0) |> List.sum |> printfn "%i"