namespace AdventOfCode

module CampCleanup =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    // helpers
    
    let firstPairFromArray ss =
        match Array.length ss with
        | l when l >= 2 -> (ss[0], ss[1])
        | _ -> failwithf $"firstPairFromArray requires at least 2 elements; found %d{ss |> Array.length}"
        
    let rangeAsSet (first, last) =
        seq { first .. last } |> Set.ofSeq
        
    let eitherOr fn (a, b) =
        (a |> fn b) || (b |> fn a)
        
    let spread fn (a, b) =
        (fn a, fn b)
    
    let contains a b =
        Set.isSubset a b
        
    let overlaps a b =
        (Set.intersect a b) <> Set.empty
        
    // parsing
    
    let parseRange (s: string) =
            s.Split("-")
            |> Array.map int
            |> firstPairFromArray
            |> rangeAsSet
        
    let inputToPairedAssignments s =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s -> s.Split(",") |> firstPairFromArray)
        |> Seq.map (spread parseRange)
        
    // part 1 ---------------
    
    let how_many_assignment_pairs_contain_each_other pairs =
        pairs
        |> Seq.where (eitherOr contains)
        |> Seq.length
    
    // part 2 ---------------
    
    let how_many_assignment_pairs_overlap pairs =
        pairs
        |> Seq.where (eitherOr overlaps)
        |> Seq.length
