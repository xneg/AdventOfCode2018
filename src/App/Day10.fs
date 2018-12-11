module Day10

open System.IO
open System.Text.RegularExpressions

type Point2D = int * int

type Star = {Position: Point2D; Velocity: Point2D}

let move n star  = {
    Position = (fst star.Position + (fst star.Velocity) * n, snd star.Position + (snd star.Velocity) * n); 
    Velocity = star.Velocity}

let borders stars = 
    let x0, y0 = (Seq.head stars).Position

    let (miX, maX, miY, maY) = stars |> Seq.fold (fun (miX, maX, miY, maY) s -> (
        (if miX > fst s.Position then fst s.Position else miX),
        (if maX < fst s.Position then fst s.Position else maX),
        (if miY > snd s.Position then snd s.Position else miY),
        (if maY < snd s.Position then snd s.Position else maY))) (x0, y0, x0, y0)
    (miX, maX, miY, maY) 

let bordersHalfPerimeter stars = 
    let (miX, maX, miY, maY) = borders stars
    (maX - miX) + (maY - miY) 

let normalize stars =
    let (miX, _ , miY, _) = borders stars
    stars |> Seq.map (fun s -> {Position = (fst s.Position - miX, snd s.Position - miY); Velocity = s.Velocity})

let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString() 

let print stars =
    let stars = normalize stars
    let (_, maX , _, maY) = borders stars    
    let points = stars |> Seq.map (fun s -> s.Position)
    let output = [for y in 0 .. maY do
                    let line = [for x in 0 ..maX do
                                yield if Seq.exists(fun p -> fst p = x && snd p = y) points then '#' else ' ']
                    yield line |> implode]
    output |> List.iter (printfn "%s")     

let foo speed stepsCount data =
    let rec step speed max (i:int) acc data =
        if (i = max) then (acc |> List.rev, data)
        else
            let newData = data |> Seq.map (move speed)
            let acc = (newData |> bordersHalfPerimeter) :: acc
            step speed max (i + 1) acc newData
    let initial = data |> bordersHalfPerimeter        
    step speed stepsCount 0 [initial] data       

let moveStars speed stepsCount data =
    let rec iniStep speed max (i: int) data =
        if (i = max) then data 
        else 
            let newData = data |> Seq.map (move speed)
            iniStep speed max (i + 1) newData
    iniStep speed stepsCount 0 data        

let moveStars1 data = moveStars 1 1 data

let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None  

let readLines (filePath: string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line
        }

let fileName = @"./data/10.txt"

let parse data =
    match data with
    | Regex @"position\=<(.+),(.+)> velocity=<(.+),(.+)>" [ x; y; vx; vy ] -> 
        {Position = (int x, int y); Velocity = (int vx, int vy)}
    | _ -> failwith data

let data = fileName |> readLines |> Seq.map parse 

moveStars 100 107 data |> moveStars 1 186 |> print

100 * 107 + 186



