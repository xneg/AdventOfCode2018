module Day04

// problem page
// http://adventofcode.com/2018/day/4

open System.Text.RegularExpressions
open System
open System.IO

let readLines (filePath: string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line
    }

//open Utilities

type RawData =
    | Begin of int * DateTime
    | Wake of DateTime
    | Sleep of DateTime

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine line =
    match line with
    | Regex @"\[(.*)\] Guard #([0-9]+)" [ date; id ] -> Begin(id |> int, date |> DateTime.Parse)
    | Regex @"\[(.*)\] falls asleep" [date] -> Sleep(date |> DateTime.Parse)
    | Regex @"\[(.*)\] wakes up" [date] -> Wake(date |> DateTime.Parse)

let rec transpose = function
    | (_::_)::_ as M -> 
        List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let setMidnightHour() = [|for _ in 0 .. 59 -> 0|]

let mutable totalMap: Map<int*int, int[]> = Map.empty

let mutable currentId = 0

let mutable sleepMinute = 0;


let sortRawData = function
    | Begin (_, date) -> date
    | Wake date -> date
    | Sleep date -> date

let fileName = @"./data/04.txt"

let parseRawDate rawData =
    match rawData with
    | Begin (id, date) -> 
        currentId <- id
        let dayOfYear = date.DayOfYear + (if date.Hour = 0 then 0 else 1)

        totalMap <- totalMap.Add((currentId, dayOfYear), setMidnightHour())
    | Sleep (date) -> 
        sleepMinute <- date.Minute
        
    | Wake (date) ->
        let dayOfYear = date.DayOfYear + (if date.Hour = 0 then 0 else 1)
        let mutable minutes = totalMap.TryFind(currentId, dayOfYear).Value
        for i in sleepMinute .. date.Minute - 1 do
            minutes.[i] <- 1

totalMap <- Map.empty
currentId <- 0
sleepMinute <- 0

let totalCalculation =
    fileName 
    |> readLines 
    |> Seq.toList
    |> List.map parseLine
    |> List.sortBy sortRawData
    |> List.iter parseRawDate

let z =
    totalMap 
    |> Map.toList 
    |> List.map (fun x -> ((fst x) |> fst, snd x |> Array.sum))
    |> List.groupBy fst
    |> List.map (fun (key, group) -> key, group |> List.sumBy snd)
    |> List.sortByDescending snd
    |> List.take 1
// 3557

let y =
    totalMap
    |> Map.filter (fun x _ -> fst x = 3557)
    |> Map.toList
    |> List.map (fun x -> snd x |> List.ofArray )
    |> transpose
    |> List.mapi (fun i x -> (i, x |> List.sum))
    |> List.sortByDescending snd
    |> List.take 1

    //30

let w =
    totalMap
    |> Map.toList 
    |> List.map (fun x -> ((fst x) |> fst, snd x |> List.ofArray))
    |> List.groupBy fst
    |> List.map (fun (key, group) -> key, group |> List.map snd |> transpose)
    |> List.map (fun (key, group) -> key, group |> List.map (fun x -> x |> List.sum))
    |> List.map (fun (key, group) -> key, group |> List.max)
    |> List.sortByDescending (fun (key, group) -> group)
    |> List.take 1

//269

let v = 
    totalMap
    |> Map.filter (fun x _ -> fst x = 269)
    |> Map.toList 
    |> List.map (fun x -> ((fst x) |> fst, snd x |> List.ofArray))
    |> List.groupBy fst
    |> List.map (fun (key, group) -> key, group |> List.map snd |> transpose)
    |> List.map (fun (key, group) -> key, group |> List.mapi (fun i x -> (i, x |> List.sum)))
    |> List.map (fun (key, group) -> key, group |> List.maxBy snd)

// 39

// 269 * 39




