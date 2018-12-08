module Day07

open Utilities

let parseLine line =
    match line with
    | Input.Regex @"Step (.) must be finished before step (.) can begin." [ parent; child ] -> (parent, child) //printfn "parent %s child %s" parent child
    | _ -> failwith "Wrong input data"

let createDependencyDictionary list =
    Seq.fold (fun (map: Map<'a, 'a list>) (v, k) ->
        if Map.containsKey k map then Map.add k ([v] @ (Map.find k map)) map
        else Map.add k [v] map)
        (Map.empty) list

let contains inner outer = 
    (inner 
    |> List.choose(fun el -> if List.contains el outer then Some(1) else None) 
    |> List.sum) = (List.length inner)

let getUnlocked dic acc =
    dic |> Map.filter (fun k v -> not (List.contains k acc) && acc |> contains v) |> Map.toList |> List.map (fun (k, v) -> k)

let initialRoots dic = 
    let allLeafes = dic |> Map.toList |> List.map (fun (k, v) -> k)
    let allNodes = (List.collect (fun (k, v) -> v) (dic |> Map.toList)) |> List.distinct

    allNodes |> List.filter (fun a -> not (allLeafes |> List.contains a))

let calculatePath data =
    let dic = data |> createDependencyDictionary

    let rec innerRec (data: 'a list) acc =
        let sortedList = data  |> List.filter (fun a -> not (acc |> List.contains a)) |> List.sort
        match sortedList with
        | [] -> acc
        | choise::tail ->
            let acc = choise :: acc
            let unlocked = getUnlocked dic acc
            let newData = tail @ unlocked
            innerRec newData acc
    innerRec (initialRoots dic)  [] |> List.rev

let fileName = @"./data/07.txt"

let data =
    fileName
    |> Input.readLines
    |> Seq.map (parseLine)

let firstPuzzle() = data |> calculatePath |> List.iter (printf "%s")
