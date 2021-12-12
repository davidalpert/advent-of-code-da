namespace AdventOfCode

open System
open AdventOfCode.Input
open AdventOfCode.utils

module Diagnostics =
  let diagnosticReportFromInput (input: string) =
    splitToTrimmedLines input
    |> Seq.map List.ofSeq
    |> List.ofSeq

  let mostCommonBit (col: char list) =
    col
    |> List.groupBy (fun c -> c)
    |> List.map (fun (bit, bits) -> (bit, bits |> List.length))
    |> List.sortByDescending (fun (b, n) -> n, (b |> int))
    |> List.head
    |> fst

  let epsilonFromGamma (gamma: string) =
    gamma.ToCharArray()
    |> Array.map (fun bit ->
        match bit with
        | '0' -> '1'
        | '1' -> '0'
        | _ -> ' ')
    |> String

  let powerConsumption report =
    let gammaBits =
        report
        |> transpose
        |> List.map mostCommonBit
        |> Array.ofList
        |> String

    let epsilonBits = gammaBits |> epsilonFromGamma

    Convert.ToInt64(gammaBits, 2)
    * Convert.ToInt64(epsilonBits, 2)

  let lifeSupportRating report =
    let bitValueOf (index: int) (row: char list) = row.[index]

    let filterByBitMatching (index: int) (list: char list list) =
        let mostCommonBits =
            list |> transpose |> List.map mostCommonBit

        let b = mostCommonBits.[index]

        list
        |> List.where (fun row ->
            if (row |> bitValueOf index) = b then
                true
            else
                false)

    let filterByBitNotMatching (index: int) (list: char list list) =
        let mostCommonBits =
            list |> transpose |> List.map mostCommonBit

        let b = mostCommonBits.[index]

        list
        |> List.where (fun row ->
            if (row |> bitValueOf index) = b then
                false
            else
                true)

    let rec loop (filter: int -> char list list -> char list list) (currentIndex: int) (remaining: char list list) =
        // printfn "loop %A %A" currentIndex remaining
        match remaining with
        | head :: [] -> Convert.ToInt64(head |> Array.ofList |> String, 2)
        | _ -> loop filter (currentIndex + 1) (filter currentIndex remaining)

    let o2rating = report |> loop filterByBitMatching 0

    let c02rating = report |> loop filterByBitNotMatching 0

    // printfn "%A" (o2rating,c02rating)

    o2rating * c02rating

