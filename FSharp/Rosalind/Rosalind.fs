namespace Rosalind

open System
open System.Text.RegularExpressions

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

    (*
        RNA : Transcribing DNA into RNA
    *)

    let transformToRna s =
        s 
        |> Seq.map (fun c -> if c = 'T' then "U" else string c)
        |> Seq.reduce (+)

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

    (*
        DBPR : Introduction to Protein Databases
    *)

    let (|Match|_|) regex str =
        let matches = Regex(regex).Matches(str)
        match matches.Count > 0 with
            | true -> Some ([for m in matches do
                                for i in 1..m.Groups.Count - 1 ->
                                    m.Groups.[i].Value])
            | false -> None
                
    let findMatches path regex =
        match readFromFile path with
            | Match regex gr -> gr |> List.reduce (fun acc elem -> acc + "\n" + elem)
            | _ -> String.Empty

[<AutoOpen>]
module ``Bioinformatics Textbook Track`` =

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

    (*
        1B : Reverse Complement Problem
    *)

    let mapComplement c =
        match c with
            | 'A' -> "T"
            | 'C' -> "G"
            | 'G' -> "C"
            | 'T' -> "A"
            | _ -> string c
 
    let revComplement pattern =
        pattern
            |> List.ofSeq
            |> List.map (fun e -> mapComplement e)
            |> List.rev
            |> List.reduce (fun a e -> a + e)

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

    (*
        BINS : Binary Search
    *)

    let binarySearch value a =
        let rec findIndex a value indx =
            let low = 0
            let high = Array.length a - 1
            let mid = low + (high - low) / 2
 
            match a with
                | [||] -> -1
                | [|el1|] -> if el1 = value then indx else -1
                | _ -> match a.[mid] with
                        | v when v = value -> mid + indx
                        | v when v < value -> let lowerBound = mid + 1
                                              findIndex a.[lowerBound..] value (lowerBound + indx)
                        | v when v > value -> findIndex a.[0..mid - 1] value indx
 
        findIndex a value 1

    let doSearch path = 
        let lines = readLines path
        let input = lines.[2].Split(' ') |> Array.map (fun e -> int e)
        let search = lines.[3].Split(' ') |> Array.map (fun e -> int e)

        let trim (s: string) =
            s.Trim()
 
        search
            |> List.ofArray
            |> List.map (fun e -> binarySearch e input)
            |> List.fold (fun a e -> sprintf "%s %s" a (string e)) String.Empty
            |> trim