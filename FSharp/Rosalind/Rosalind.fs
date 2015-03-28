﻿namespace Rosalind

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