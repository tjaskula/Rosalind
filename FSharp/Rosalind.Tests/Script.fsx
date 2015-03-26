// sets the current directory to be same as the script directory
System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)
#I @"../packages/FsCheck.1.0.4/lib/net45"
#I @"../packages/FsCheck.Xunit.1.0.4/lib/net45"
#I @"../packages/xunit.1.9.2/lib/net20"
#I @"bin/Debug"
#r @"FsCheck.dll"
#r @"FsCheck.XUnit.dll"
#r @"xunit.dll"
#r @"Euler.dll"

open FsCheck
open Euler.Problems

let rec divisors acc number =
    let count, indx = acc
    if pown indx 2 <= number then
        if number % indx = 0L then
            divisors (count + 2L, indx + 1L) number
        else
            divisors (count, indx + 1L) number
    else
        count

Seq.unfold (fun (a, state) -> Some(a, (a + state, state + 1L))) (1L, 2L)
    |> Seq.skipWhile (fun elem -> divisors (2L, 2L) elem < 500L)
    |> Seq.take 1
    |> Seq.max