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
    | _ -> failwith "Wrong input data"

let sortRawData = function
    | Begin (_, date) -> date
    | Wake date -> date
    | Sleep date -> date

let getSleepInterval minute1 minute2 = [for i in minute1 .. minute2 - 1 -> i]

let parseRawData l =
    let rec innerParse aggregator currentId rawData =
        match rawData with
        | Begin (id, _) :: tail -> innerParse aggregator id tail
        | [Sleep(date1);Wake(date2)] -> 
            let interval = getSleepInterval date1.Minute date2.Minute 
            [(currentId, interval)] @ aggregator
        | Sleep (date1) :: Wake (date2) :: tail ->
            let interval = getSleepInterval date1.Minute date2.Minute
            let newAggregator =[(currentId, interval)] @ aggregator
            innerParse newAggregator currentId tail
        | _ -> failwith "Invalid data order"
    innerParse List.empty 0 l    

let createMap list =
    Seq.fold (fun (map: Map<_, int>) c ->
        if Map.containsKey c map then Map.add c ((Map.find c map) + 1) map
        else Map.add c 1 map)
        (Map.empty) list    

let fileName = @"./data/04.txt"

let sleepIntervals = 
    fileName 
    |> readLines 
    |> Seq.toList
    |> List.map parseLine
    |> List.sortBy sortRawData
    |> parseRawData

let firstPuzzle = 
    let (id, _, minute) =
        sleepIntervals
            |> List.groupBy fst
            |> List.map (fun (k, v) -> (k, v |> List.map snd |> List.concat))
            |> List.map (fun (k, v) -> (k, v |> List.length, v|> createMap |> Map.toList |> List.sortByDescending snd |> List.head |> fst))
            |> List.sortByDescending (fun (_, total, _) -> total)
            |> List.head
    id * minute        

let secondPuzzle =
    let (id, (minute, _)) =
        sleepIntervals
        |> List.groupBy fst
        |> List.map (fun (k, v) -> (k, v |> List.map (fun x -> snd x) |> List.concat))
        |> List.map (fun (k, v) -> (k, v |> createMap |> Map.toList |> List.sortByDescending snd |> List.head))
        |> List.sortByDescending (fun (k, v) -> snd v)
        |> List.head
    id * minute

