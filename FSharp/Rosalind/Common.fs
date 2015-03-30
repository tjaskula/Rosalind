namespace Rosalind

[<AutoOpen>]
module Common =
    
    [<AutoOpen>]
    module FileHelper =
    
        open System.IO

        let readFromFile path =
            File.ReadAllText(path).Trim()

        let readLines path =
            File.ReadLines(path) |> Array.ofSeq