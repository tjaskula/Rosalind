namespace Rosalind

open System

[<AutoOpen>]
module ``Bioinformatics Stronghlod`` =

    (*
        DNA	: Counting DNA Nucleotides
    *)
    let countACGT currentCount el =
        let a, c, g, t = currentCount
        match el with
            | 'A' -> a + 1, c, g, t
            | 'C' -> a, c + 1, g, t
            | 'G' -> a, c, g + 1, t
            | 'T' -> a, c, g, t + 1
            | _ -> currentCount
    

    let dna (rawDna : string) =
        rawDna.ToCharArray()
           |> Array.fold (fun count elem -> countACGT count elem) (0, 0, 0, 0)

    // 2
    let dna' (rawDna : string) =
        rawDna
        |> Seq.countBy (fun key -> key)
        |> Seq.sortBy (fun (key, _) -> key)
        |> Seq.map (fun (_, count) -> count)
        |> Seq.toList

    // 3
    let dna'' (rawDna : string) =
        rawDna
        |> Seq.groupBy (fun e -> e)
        |> Seq.map (fun (e, elems) -> e, Seq.length elems)
        |> Seq.toList

[<AutoOpen>]
module ``Bioinformatics Armory`` =

    (*
        INI : Introduction to the Bioinformatics Armory
    *)
    let ini (rawDna : string) =
        rawDna
        |> Seq.countBy (fun key -> key)
        |> Seq.sortBy (fun (key, _) -> key)
        |> Seq.map (fun (_, count) -> string count)
        |> Seq.reduce (fun  acc elem -> acc + " " + elem)