namespace AdventOfCode

module CalorieCounting =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    let fromInput (s: string) =
        s
        // |> Seq.map (fun s ->
        //     // printfn "s: %s" s
        //     s)
        // |> Array.ofSeq
