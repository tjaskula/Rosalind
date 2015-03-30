namespace Rosalind

[<AutoOpen>]
module Common =
    
    open System.IO

    let readFromFile path =
        File.ReadAllText(path)