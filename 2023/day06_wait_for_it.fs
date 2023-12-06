namespace AdventOfCode

module day06_Wait_For_It =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    type RaceRecord =
        { t : int64; d : int64 }

        member this.allStartingOptions =
            seq { 0L .. this.t }
            |> Seq.map (fun t ->
                    let speed = t
                    let remainingTime = this.t - t
                    { t = t; d = remainingTime * speed }
                )
        
        member this.winningRaces =
            this.allStartingOptions
            |> Seq.filter (fun r -> r.d > this.d)

    type RaceDocument =
        { races : RaceRecord array }

        member this.part1NumberOfWaysToWin =
            this.races
            |> Seq.map (fun r -> r.winningRaces |> Seq.length)

        member this.part1ProductOfNumberOfWaysToWin =
            this.part1NumberOfWaysToWin
            |> Seq.fold (*) 1
 
    module parser =
        let ws = spaces
        let ch = pchar

        let pTimes =
            %% ws -- %"Time:" -- ws -- +.((pint32 .>> ws)* qty[1..])
            -%> auto

        let pDistances =
            %% ws -- %"Distance:" -- ws -- +.((pint32 .>> ws) * qty[1..])
            -%> auto

        let pInput =
            %% ws -- +.pTimes -- ws -- +.pDistances
            -|> fun tt dd ->
                Seq.zip tt dd
                |> Seq.map (fun (t,d) -> { t = t; d = d; })
                |> Array.ofSeq
                |> fun a -> { races = a }
                
        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse pInput input

        let part2ParseInput =
            %% ws -- "Time:" -- +.(restOfLine true)
            -- ws -- "Distance:" -- +.(restOfLine true)
            -|> (fun ts ds ->
                let t = ts.Replace(" ", "") |> Int64.Parse
                let d = ds.Replace(" ", "") |> Int64.Parse
                { t = t; d = d }
            )
            
        let part2MustParse (input:string) =
            match run part2ParseInput input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Array.ofSeq
