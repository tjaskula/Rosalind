// sets the current directory to be same as the script directory
System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)
#I @"../packages/FsCheck.1.0.4/lib/net45"
#I @"../packages/FsCheck.Xunit.1.0.4/lib/net45"
#I @"../packages/xunit.1.9.2/lib/net20"
#I @"../packages/FSharp.Data.2.2.0/lib/net40"
#I @"bin/Debug"
#r @"FsCheck.dll"
#r @"FsCheck.XUnit.dll"
#r @"xunit.dll"
#r @"Rosalind.dll"
#r @"System.Net.Http.dll"
#r @"System.Xml.Linq.dll"
#r @"FSharp.Data.dll"
 
open Rosalind.``Bioinformatics Stronghlod``
open Rosalind.``Bioinformatics Armory``
open Rosalind.``Bioinformatics Textbook Track``
open Rosalind.Common
 
open System.Text.RegularExpressions
open System.Xml.Linq
open System.Net.Http
open System.Net
open FSharp.Data
 
// 1C : Pattern Matching Problem
 
let indexOfMatches d p =
    [for m in Regex.Matches(d, @"(?<=" + p + ")") ->
        m.Index - String.length p]
 
let lines = readLines "../Data/pattern_matching.txt"
let pattern = lines.[0]
let input = lines.[1]
 
indexOfMatches input pattern
    |> Seq.map (fun e -> string e)
    |> Seq.reduce (fun a e -> sprintf "%s %s" a e)