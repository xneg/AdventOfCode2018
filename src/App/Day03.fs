module Day03

// problem page
// http://adventofcode.com/2018/day/3

open System.Text.RegularExpressions
open Utilities

type Point =
    {
        X: int;
        Y: int;
    }

type Claim =
    {
        Id: int;
        Position: Point;
        Width: int;
        Height: int;
    }

let createClaim str =
    let parsed = Regex.Split(str, @"\D+")
    { 
        Id = (int)parsed.[1];
        Position = {
            X = (int)parsed.[2];
            Y = (int)parsed.[3];}
        Width = (int)parsed.[4];
        Height = (int)parsed.[5];}

let filledPoints claim =
    [for i in [1..claim.Width] do
        for j in [1..claim.Height] -> (claim.Id, {X = claim.Position.X + i; Y = claim.Position.Y + j})]

let fileName = @"./data/03.txt"

let claims = fileName |> Input.readLines |> Seq.map createClaim

let filledMap = 
    claims
    |> Seq.map filledPoints 
    |> Seq.concat
    |> Seq.fold (fun (hash:Map<Point, List<int>>) p ->
        let v = fst p
        let key = snd p
        if Map.containsKey key hash then Map.add key ((Map.find key hash)@[v]) hash
        else Map.add key ([v]) hash)
        (Map.empty)

let firstPuzzle() =
    filledMap
    |> Map.filter (fun _ v -> List.length v > 1) 
    |> Map.count
    |> printfn "%i"

let secondPuzzle() =
    let totalSum = claims |> Seq.sumBy (fun c -> c.Id)
    let sum = 
        filledMap
        |> Map.filter (fun _ v -> List.length v > 1) 
        |> Map.toList
        |> List.map snd
        |> List.concat
        |> List.distinct
        |> List.sum

    totalSum - sum |> printfn "%i"    

