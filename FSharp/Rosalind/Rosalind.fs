namespace Rosalind

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

[<AutoOpen>]
module ``Bioinformatics Textbook Track`` =

    open System.Text.RegularExpressions

    (*
        1A : Frequent Words Problem
    *)
    
    let patternize size (d : string) =
        seq {for i in 0..d.Length - size do
                yield d.Substring(i, size)}
                    |> Seq.distinct
 
    let countMatches d p =
        Regex.Matches(d, @"(?<=" + p + ")").Count
 
    let matches data p = 
                p
                |> List.ofSeq
                |> List.map (fun p -> p, countMatches data p)
                |> List.sortBy (fun (_, count) -> count * -1)
 
    let max m = 
            m
            |> List.head
            |> snd
 
    let mostFrequent size (data : string) =
        let matched = data
                        |> patternize size
                        |> matches data
        let m = matched |> max

        matched
            |> List.filter (fun (_, count) -> count = m)
            |> List.map (fun m -> fst m)
            |> List.reduce (fun acc el -> sprintf "%s %s" acc el)

[<AutoOpen>]
module ``Algorithmic Heights`` =
    
    (*
        FIBO : Fibonacci Numbers
    *)

    let rec fibo n =
        match n with
            | 0 -> 0
            | 1 -> 1
            | _ -> fibo(n - 1) + fibo(n - 2)