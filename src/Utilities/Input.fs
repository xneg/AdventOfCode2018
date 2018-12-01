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

    let explode (s:string) =
        [for c in s -> c]