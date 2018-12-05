namespace Utilities

module Input = 
    open System.IO

    let processFile (filePath : string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line.Split([|'\t';' '|])
        }

    let readLines (filePath: string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line
        }

    let explode (s:string) =
        [for c in s -> c]

    let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()    

    let createMap list =
        Seq.fold (fun (map: Map<_, int>) c ->
            if Map.containsKey c map then Map.add c ((Map.find c map) + 1) map
            else Map.add c 1 map)
            (Map.empty) list        