namespace AdventOfCode

module CampCleanup =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    let firstPairFromArray ss =
        match Array.length ss with
        | l when l >= 2 -> (ss[0], ss[1])
        | _ -> failwithf $"firstPairFromArray requires at least 2 elements; found %d{ss |> Array.length}"
        
    let rangeAsSet (first, last) =
        seq { first .. last } |> Set.ofSeq
        
    let parseRange (s: string) =
            s.Split("-")
            |> Array.map int
            |> firstPairFromArray
            |> rangeAsSet
        
    let contains a b =
        Set.isSubset a b
        
    let overlaps a b =
        (Set.intersect a b) <> Set.empty
            
    let inputToPairedAssignments s =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s -> s.Split(",") |> firstPairFromArray)
        |> Seq.map (fun (left, right) -> (left |> parseRange, right |> parseRange))
        
    // part 1 ---------------
    
    let how_many_assignment_pairs_contain_each_other pairs =
        pairs
        |> Seq.where (fun (left, right) -> (left |> contains right) || (right |> contains left))
        |> Seq.length
    
    // part 2 ---------------
    
    let how_many_assignment_pairs_overlap pairs =
        pairs
        |> Seq.where (fun (left,right) -> (left |> overlaps right) || (right |> overlaps left))
        |> Seq.length
