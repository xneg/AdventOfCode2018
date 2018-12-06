module Day06

let outerBounds data = 
    let x1 = data |> List.minBy (fst) |> fst
    let y1 = data |> List.minBy (snd) |> snd
    let x2 = data |> List.maxBy (fst) |> fst
    let y2 = data |> List.maxBy (snd) |> snd
    (x1 - 1, y1 - 1, x2 + 1, y2 + 1)

let manhatten a b = abs(fst a - fst b) + abs(snd a - snd b)

let getNearestId pos points =
    let [x; y] =
        points 
        |> Seq.mapi (fun i x -> (i, manhatten pos x))
        |> Seq.sortBy snd
        |> Seq.take 2    
        |> Seq.toList
    if snd x <> snd y then fst x else -1

let getTotal pos points = points |> Seq.map (fun x -> manhatten pos x) |> Seq.sum

let calcField (x1, y1, x2, y2, data) f =
    [for x in x1 .. x2 do
        for y in y1 .. y2 ->
            ((x, y), f (x, y) data)]

let firstPuzzleResult data =
    let x1, y1, x2, y2 = outerBounds data
    let field = calcField (x1, y1, x2, y2, data) getNearestId

    let excluded =
        field 
        |> List.filter (fun ((x, y), _) -> x = x1 || x = x2 || y = y1 || y = y2) 
        |> List.map snd
        |> List.distinct
    
    field 
        |> List.map (snd) 
        |> List.groupBy (fun x -> x) 
        |> List.map (fun x -> (fst x, snd x |> List.sumBy (fun _ -> 1)))
        |> List.filter (fun x -> fst x <> -1)
        |> List.filter (fun x -> not (List.contains (fst x) excluded))
        |> List.sortByDescending (snd)
        |> List.head |> snd

let secondPuzzleResult limit data =
    let left, top, width, height = outerBounds data
    let field = calcField (left, top, width, height, data) getTotal

    field |> List.map snd |> List.filter (fun x -> x < limit) |> List.sumBy (fun x -> 1)    

let processFile (filePath : string) =
        seq {
            use fileReader = new System.IO.StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line.Split([|'\t';' ';','|])
        }

let fileName = @"./data/06.txt"

let data = 
    fileName 
    |> processFile 
    |> Seq.map (fun x -> (int x.[0], int x.[2]))
    |> Seq.toList

let firstPuzzle() = data |> firstPuzzleResult

let secondPuzzle() = data |> secondPuzzleResult 10000