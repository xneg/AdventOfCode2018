module Day07

open Utilities

let parseLine line =
    match line with
    | Input.Regex @"Step (.) must be finished before step (.) can begin." [ parent; child ] -> (char parent, char child) //printfn "parent %s child %s" parent child
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

    let rec innerRec data acc =
        let sortedList = data |> List.sort  |> List.filter (fun a -> not (acc |> List.contains a)) |> List.sort
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

let firstPuzzle() = data |> calculatePath |> List.iter (printf "%c")

let calculateTime workersCount secondsCount data  =
    let getNToWork n list = list |> List.sort |> List.truncate n |> List.map (fun x -> (x, int x - 64 + secondsCount, 1))

    let dic = data |> createDependencyDictionary

    let initial = initialRoots dic |> getNToWork workersCount

    let rec innerRec atWork finished acc =
        let listDone, atWork = atWork |> List.partition (fun (_, total, step) -> step = total)
        let finished = (listDone |> List.map (fun (x, _, _) -> x)) @ finished
        let atWork = atWork |> List.map (fun (x, total, step) -> (x, total, step + 1))
        let newCount = workersCount - List.length atWork

        let atWorkList = atWork |> List.map (fun (x, _, _) -> x)
        let newly = getUnlocked dic finished |> List.filter (fun el -> not (atWorkList |> List.contains el)) |> getNToWork newCount

        let atWork = newly @ atWork
        let acc = acc + 1
        if (List.isEmpty atWork) then acc else innerRec atWork finished acc                            

    innerRec initial [] 0

let secondPuzzle() = data |> calculateTime 5 60 |> printfn "%i"
 
        
