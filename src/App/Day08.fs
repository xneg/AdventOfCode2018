module Day08

open Utilities
let input = [2; 3; 0; 3; 10; 11; 12; 1; 1; 0; 1; 99; 2; 1; 1; 2]

let mutable removed = 0

type 'T tree = 
    | Node of 'T * ('T tree list)
    | Leaf of 'T


let rec mark data acc parentN childCurrentN childLastN  =
    if childCurrentN = -1 then

    match data with
    | childsCount::metaCount::tail ->
        if childsCount = 0 then
            let acc = [tail |> List.truncate metaCount |> List.map(fun x -> (x, childCurrentN))] @ acc
            let tail = tail |> List.skip metaCount
            if childCurrentN = childLastN then
                mark tail acc parentN -1 -1

// let rec calculate2 data tree =
//     match data with
//     | [] -> []
//     |childsCount::metaCount::tail ->
//         if childsCount = 0 then
//             let meta = tail |> List.truncate metaCount
//             Leaf(meta), tail |> List.skip metaCount
//         else
//             // let mutable i = 0
//             // let mutable l: int list = tail
//             // while i < childsCount do
//             //     let newTail = l |> calculate2
//             //     l <- newTail
//             //     i <- i + 1
//             // l |> List.skip metaCount            
//     |[_] -> failwith "Wrong input data" 

let addNode (t: 'T tree) node =
    match t with
    | Node(a, list) -> Node(a, node::list)
    | Leaf(a) -> Node(a, [node])

// let rec xxx data nth childsCount (tr: int tree) =
//     match data with
//     | [] -> []
//     |childsCount::metaCount::tail ->
//         if childsCount = 0 then
//             let result = tail |> List.truncate metaCount |> List.sum
//             let tail = tail |> List.skip metaCount
//             if nth = childsCount then
//                 tail, (addNode tr result)
//             else
//                 xxx tail (nth + 1) childsCount (addNode tr result)
//         else
//             if lastChild then
//             else
//                 // addNode tr 
//     |[_] -> failwith "Wrong input data"                  

let rec calculate acc what data  =
    match data with
    | [] -> acc, []
    |childsCount::metaCount::tail ->
        let acc = acc + childsCount + metaCount
        if childsCount = 0 then
            acc, tail |> List.skip metaCount
        else
            let mutable i = 0
            let mutable l = tail
            while i < childsCount do
                 let (acc, newTail) = l |> calculate acc true
                 l <- newTail
                 i <- i + 1
            acc, l |> List.skip metaCount            
    |[_] -> failwith "Wrong input data"  

// calculate 0 input

// let processFile (filePath : string) =
//         seq {
//             use fileReader = new StreamReader(filePath)

//             while not fileReader.EndOfStream do
//                 let line = fileReader.ReadLine()
//                 yield line.Split([|'\t';' '|])
//         }
// let fileName = @"./data/08.txt"


// let firstPuzzle() =
//     let data =
//         fileName
//         |> Input.processFile 
//         |> Seq.head
//         |> Array.map (int)
//         |> Array.toList 
    
//     //let data = input

//     data |> calculate
//     ((data |> List.sum) - removed) |> printfn "%i"

// let data =
//         fileName
//         |> processFile 
//         |> Seq.head
//         |> Array.map (int)
//         |> Array.toList 

// data |> List.sum  //55813

// removed <- 0
// data |> calculate2

// 55813 - 43752



// let rec calculate acc data =
//     match data with
//     | [] -> (acc, [])
//     |childsCount::metaCount::tail ->
//         if childsCount = 0 then
//             let metas = tail |> List.truncate metaCount |> List.sum
//             (metas, tail |> List.skip metaCount)
//         else
//             let mutable i = 0
//             let mutable l: int list = tail
//             let childMetas = [while i < childsCount do
//                                 let (metaSum, newTail) = l |> calculate 0
//                                 l <- newTail
//                                 i <- i + 1
//                                 yield metaSum] |> List.sum
//             let metas = l |> List.truncate metaCount |> List.sum
//             (metas + childMetas, l)
//     |[_] -> failwith "Wrong input data"  
    