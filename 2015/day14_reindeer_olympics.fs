namespace AdventOfCode

module ReindeerOlympics =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    [<Measure>]
    type kilometer
    // conversion constant
    let metersPerKilometer = 1000.0<meter/kilometer>
    // conversion functions
    let convertMetersToKilometers (m: float<meter>) = m / metersPerKilometer
    let convertKilometersToMeters (km: float<kilometer>) = km * metersPerKilometer

    type Stats =
        { name: string
          flySpeed: float<kilometer / second>
          flyDuration: float<second>
          restDuration: float<second> }
        member this.cycleDuration = this.flyDuration + this.restDuration
        member this.distancePerCycle = this.flyDuration * this.flySpeed

        member this.cyclesCompletedAfterNSeconds(n: int<second>) =
            Math.Floor(((n |> float) * 1.0<second>) / this.cycleDuration)
            |> int

        member this.distanceAfterNSeconds(n: int<second>) =
            let leftoverSeconds = ((n |> float) * 1.0<second>) % this.cycleDuration

            let leftoverSpentFlying =
                Math.Min(leftoverSeconds / 1.0<second>, this.flyDuration / 1.0<second>)
                * 1.0<second>

            let distanceTraveledBeforeTheCurrentCycle =
                this.distancePerCycle
                * (this.cyclesCompletedAfterNSeconds (n) |> float)

            let distanceTravledThisCycle = this.flySpeed * leftoverSpentFlying

            distanceTraveledBeforeTheCurrentCycle
            + distanceTravledThisCycle

    let parseOneStat (s: string) =
        match s with
        | Regex @"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds."
                [ name; fSpeed; fDuration; rDuration ] ->
            { name = name
              flySpeed = (fSpeed |> float) * 1.0<kilometer/second>
              flyDuration = (fDuration |> float) * 1.0<second>
              restDuration = (rDuration |> float) * 1.0<second> }
        | _ -> raise (System.Exception(sprintf "could not parse: '%s'" s))

    let parseStats (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map parseOneStat
        |> Array.ofSeq

    let competeForNSeconds (n: int<second>) (stats: Stats []) =
        stats
        |> Array.map (fun s -> (s.name, s.distanceAfterNSeconds n))
        |> Map.ofArray

    let competeForNSeconds2 (n: int<second>) (stats: Stats []) =
        let points =
            Dictionary<string, int>(stats |> Seq.map (fun s -> (s.name, 0)) |> dict)

        let folder (p: IDictionary<string, int>) (i: int) =
            let currentDistanceByName = stats |> competeForNSeconds (i * 1<second>)

            let leaderDistance =
                currentDistanceByName
                |> Seq.maxBy (fun pair -> pair.Value)
                |> (fun pair -> pair.Value)

            currentDistanceByName
            |> Seq.filter (fun pair -> pair.Value = leaderDistance)
            |> Seq.iter (fun pair -> p.[pair.Key] <- p.[pair.Key] + 1)

            p

        seq { 1 .. (n / 1<second>) }
        |> Seq.fold folder points
// |> Seq.map (fun pair -> pair.Key, pair.Value)
// |> Seq.maxBy snd
// |> snd
