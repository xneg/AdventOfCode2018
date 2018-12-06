module Day06

type Point = int * int

let input = [(1, 1);(1, 6);(8, 3);(3, 4);(5, 5);(8, 9)]

let bounds (data : Point list) = 
    let left = data |> List.minBy (fst) |> fst
    let top = data |> List.minBy (snd) |> snd
    let width = (data |> List.maxBy (fst) |> fst) - left
    let height = (data |> List.maxBy (snd) |> snd) - top
    ((left, top), (width, height))

input |> bounds

