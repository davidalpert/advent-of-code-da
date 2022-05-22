namespace AdventOfCode

module LikeaGIFForYourYard =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open AdventOfCode.FireHazard
    open FSharp.Data.UnitSystems.SI.UnitNames

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.mapi (fun row line ->
            line
            |> Seq.mapi (fun col c ->
                match c with
                // beware off-by-one; let's use 1-indexed coords (light positions from 1 to 100)
                | '#' -> Some({ x = col + 1; y = row + 1 })
                | '.' -> None
                | _ -> raise (Exception(sprintf "unrecognized char '%c'" c)))
            |> Seq.choose id)
        |> Seq.concat
        |> Set.ofSeq

    let numberOfLitBulbs (litBulbs: Set<BulbLocation>) : int = litBulbs.Count

    let bulbIsInRange (dim: int) (b: BulbLocation) : bool =
        1 <= b.x && b.x <= dim && 1 <= b.y && b.y <= dim

    let validNeighborsOf (dim: int) (b: BulbLocation) : Set<BulbLocation> =
        seq {
            for dy in -1 .. 1 do
                for dx in -1 .. 1 -> { b with x = b.x + dx; y = b.y + dy }
        }
        |> Seq.filter (fun n -> n <> b) // self is not a neighbor
        |> Seq.filter (bulbIsInRange dim)
        |> Set.ofSeq

    let litNeighbors (dim: int) (lit: Set<BulbLocation>) (b: BulbLocation) =
        Set.intersect lit (b |> (validNeighborsOf dim))

    let nextState (dim: int) (litBefore: Set<BulbLocation>) : Set<BulbLocation> =
        let staysOn (b: BulbLocation) : bool =
            let n = (b |> (litNeighbors dim litBefore)).Count
            n = 2 || n = 3

        let turnsOn (b: BulbLocation) : bool =
            let n = (b |> (litNeighbors dim litBefore)).Count
            n = 3

        let neighborsOfLitBefore =
            litBefore
            |> Seq.collect (validNeighborsOf dim)
            |> Set.ofSeq

        let remainingOn = litBefore |> Seq.filter staysOn
        let turningOn = neighborsOfLitBefore |> Seq.filter turnsOn

        Seq.concat [ remainingOn; turningOn ] |> Set.ofSeq

    let renderSet (lit: Set<BulbLocation>) =
        lit
        |> Seq.map (fun b ->
            printfn "{row:%d;col:%d}" b.y b.x
            b)
        |> Set.ofSeq

    let renderAsGrid (dim: int) (lit: Set<BulbLocation>) =
        let border =
            seq { 1..1..dim }
            |> Seq.map (fun _ -> '-')
            |> Array.ofSeq
            |> String

        printfn "%s" border

        seq { 1..1..dim }
        |> Seq.map (fun row ->
            seq { 1..1..dim }
            |> Seq.map (fun col ->
                match lit.Contains({ x = col; y = row }) with
                | true -> '#'
                | false -> '.')
            |> Array.ofSeq
            |> String)
        |> Seq.iter (fun s -> printfn "%s" s)

        printfn "%s" border

        lit

    let stateAfterNIterations (n: int) (dim: int) (litBefore: Set<BulbLocation>) : Set<BulbLocation> =
        seq { 1..1..n }
        |> Seq.fold (fun s _ -> s |> (nextState dim)) litBefore
