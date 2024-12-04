namespace AdventOfCode

module day02_Red_Nosed_Reports =

    open System
    open System.Collections.Generic
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    module parser =
        let ws = spaces
        let ch = pchar

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse ws input

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun (s:string) ->
               s.Trim().Split(' ')
               |> Array.map int
           )
        |> Array.ofSeq


    let calculateGaps (input:int seq) =
        input
        |> Seq.windowed 2
        |> Seq.map (fun pairs ->
               pairs[1] - pairs[0]
           )
        
    let part1ReportIsSafe (levels:int seq) =
        let gaps = levels |> calculateGaps
        let firstGap = gaps |> Seq.head
        let gapsAreIncreasing = firstGap > 0
        
        gaps |> Seq.forall (fun gap ->
            let directionMatches = if gapsAreIncreasing then gap > 0 else gap < 0
            directionMatches && 1 <= (abs gap) && (abs gap) <= 3
        )
        
    // let calculateLevelSets (input:int array) =
    //     [|
    //         [| input |]
    //         input |> Array.map (fun n ->
    //            input
    //            |> Array.map (fun j -> if j = n then None else Some(j))
    //            |> Array.filter Option.isSome
    //            |> Array.map Option.get
    //         )
    //     |]
    //     |> Array.concat
        
    let generateDampenedPossibilities (input:int array) =
        Seq.unfold (fun n ->
            let p = input
                    |> Array.mapi (fun i j -> if i = n then None else Some(j))
                    |> Array.filter Option.isSome
                    |> Array.map Option.get
                    
            if n > input.Length then None else Some(p, n+1)
        ) 0
            
    let part2ReportIsSafe (levels:int array) =
        if levels |> part1ReportIsSafe then
            true
        else
            levels
            |> generateDampenedPossibilities
            |> Seq.exists part1ReportIsSafe
            
    // levels
    // |> calculateLevelSets
    // |> Array.contains part1ReportIsSafe
    //      
    //     let gapsAreIncreasing = gaps.[0] > 0
    //     
    //     gaps |> Array.forall (fun gap ->
    //         let directionMatches = if gapsAreIncreasing then gap > 0 else gap < 0
    //         directionMatches && 1 <= (abs gap) && (abs gap) <= 3
    //     )
        
    let countSafe (safeFn:int array -> bool) (reports:int array array)  =
        reports
        |> Array.filter safeFn
        |> Array.length
        
    