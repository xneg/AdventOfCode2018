module Day11
open System.Collections.Generic

let cellPower (x, y) serialNumber =
    let getHundredDigit x = (x % 1000) / 100

    let rackID = x + 10
    ((rackID * y + serialNumber) * rackID |> getHundredDigit) - 5

let calcGroupPower (x, y) size serialNumber =
    [for i in x .. x + size - 1 do
        for j in y .. y + size - 1 ->
            cellPower (i, j) serialNumber] |> List.sum

let serialNumber = 4455

let firstPuzzle() = 
    [for x in 1 .. 298 do
        for y in 1 .. 298 ->
            ((x, y), calcGroupPower(x, y) 3 serialNumber)] |> List.sortByDescending snd |> List.head

let powerField = Array2D.init 300 300 (fun i j -> cellPower (i, j) serialNumber)

let memo = new Dictionary<int * int * int, int>()

let calcPower (x, y) size =
    if size = 1 then 
        memo.Add((x, y, 1), powerField.[x, y])
        powerField.[x, y]
    else
        let row = [for i in x .. x + size - 1 -> powerField.[i, y + size - 1]] |> List.sum 
        let col = [for j in y .. y + size - 2 -> powerField.[x + size - 1, j]] |> List.sum
        let result = memo.[x, y, size - 1] + row + col
        memo.Add((x, y, size), result)
        result

let secondPuzzle() = 
    memo.Clear()
    let x, y, size = 
        [for x in 0 .. 299 do
            for y in 0 .. 299 do
                let maxSize = min (300 - x) (300 - y)
                for size in 1 .. maxSize -> ((x, y, size), calcPower (x, y) size)] 
        |> List.sortByDescending snd |> List.head |> fst
    printfn "%i,%i,%i" x y size    

