namespace AdventOfCode

module day01_Historian_Hysteria =

    open System
    open System.Collections.Generic
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    module parser =
        let ws = spaces
        let ch = pchar

        let pLocationID = pint32

        let pLocationPair =
            %% +.pLocationID -- ws -- +.pLocationID -- ws
            -%> auto

        let pListOfLocationPairs =
            %% +.(pLocationPair * qty.[1..])
            -|> fun cc -> cc |> Array.ofSeq

        let parseInput (input:string) =
            mustParse pListOfLocationPairs (input.Trim())

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

    let locationPairsToSeparateLists (pairs: (int32 * int32) array) =
        pairs |> Array.map fst, pairs |> Array.map snd

    let pairBySmallestToLargest (a: int32 array, b: int32 array) =
        Array.zip (a |> Array.sort) (b |> Array.sort)

    let sumUpDistancesApart (pairs: (int32 * int32) array) =
        pairs
        |> Array.map (fun (a, b) -> Math.Abs(a - b))
        |> Array.sum

    let calculateDistanceBetweenTwoLists (input: string) =
        input
        |> parser.parseInput
        |> locationPairsToSeparateLists
        |> pairBySmallestToLargest
        |> sumUpDistancesApart

    let calculateSimilarityScore (input: string) =
        input
        |> parser.parseInput
        |> locationPairsToSeparateLists
        |> fun (a, b) ->
            let numberOfSecondaryOccurances n =
                b |> Array.filter (fun x -> x = n) |> Array.length

            let similarity n =
                n * numberOfSecondaryOccurances n

            a |> Array.fold (fun acc x -> acc + (x |> similarity)) 0

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            printfn "s: %s" s
            s)
        |> Array.ofSeq
