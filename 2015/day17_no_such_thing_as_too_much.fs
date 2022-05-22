namespace AdventOfCode

module NoSuchThingAsTooMuch =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Seq.map int
        |> Array.ofSeq

    let differentWaysToStoreNLitres (n: int) (containers: int []) =
        let addsUpToN (ll: int list) = (ll |> List.sum) = n

        containers
        |> List.ofArray
        |> allCombinations
        |> List.filter addsUpToN
        |> Array.ofList
        |> Array.map (fun ll -> ll |> Array.ofList)
