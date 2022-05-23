namespace AdventOfCode

module MedicineforRudolph =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    let mapRenderEach (col: seq<'a>) : seq<'a> =
        col
        |> Seq.map (fun x ->
            printfn "> %A" x
            x)

    let renderEach (col: seq<'a>) : seq<'a> =
        col |> Seq.iter (fun x -> printfn "> %A" x)
        col

    type Machine(replacements: array<string * string>, calibrationString: string) =
        let generateAllFabrications (initialSet: Set<string>) =
            initialSet
            |> Seq.collect (fun m ->
                replacements
                |> Array.collect (fun (a, b) ->
                    let l = a.Length

                    m
                    |> Seq.windowed l
                    |> Seq.mapi (fun i s -> (s |> String, i))
                    |> Seq.filter (fun (s, _) -> s = a)
                    // |> Seq.map (fun s ->
                    //     printfn "> %A" s
                    //     s)
                    |> Seq.map snd
                    |> Seq.map (fun i -> m.[0 .. (i - 1)] + b + m.[(i + l) ..])
                    // |> Seq.map (fun s ->
                    //     printfn "> %A" s
                    //     s)
                    |> Array.ofSeq))
            |> Set.ofSeq

        member this.calibrationSet =
            [| calibrationString |]
            |> Set.ofSeq
            |> generateAllFabrications

        member this.replacements = replacements
        member this.calibrationString = calibrationString

        member this.sortedReplacements =
            this.replacements
            |> Array.sortByDescending (fun (_, b) -> b.Length)

        member this.cheapestConstructionStepsTopDown(molecule: string) =
            // steps with an element and more molecules cannot be simplified to a single element
            let validStep (s: string) : bool = not (s.Contains('e') && s.Length > 1)

            let possibleNextSteps (lastPossibilities: string seq) =
                lastPossibilities
                |> Seq.collect (fun anS ->
                    this.sortedReplacements
                    |> Seq.collect (fun (a, b) ->
                        let l = b.Length

                        anS
                        |> Seq.windowed l
                        |> Seq.mapi (fun i s -> (i, s |> String))
                        |> Seq.filter (fun (i, s) -> s = b)
                        |> Seq.map (fun (i, _) -> anS.[0 .. (i - 1)] + a + anS.[(i + l) ..]))
                    // |> mapRenderEach
                    |> Seq.filter validStep
                    |> Set.ofSeq)
            // |> Array.ofSeq
            // |> Array.sortBy (fun s -> s.Length)

            let generator (lastPossibilities: string seq, lastDepth) =
                // printfn "depth '%d'; considering '%d' possibilities..." lastDepth lastPossibilities.Length

                match lastPossibilities
                      |> Seq.tryFind (fun s -> s = "e")
                    with
                | Some (p) -> None // nothing more to do
                | None ->
                    // keep looking
                    Some(lastDepth, (lastPossibilities |> possibleNextSteps, lastDepth + 1))

            ([| molecule |] |> Seq.ofArray, 0)
            |> Seq.unfold generator
            |> mapRenderEach

        // https://www.reddit.com/r/adventofcode/comments/num2k6/comment/h0yi5ue/?utm_source=share&utm_medium=web2x&context=3
        member this.cheapestConstructionStepsTopDownGreedy(molecule: string) =
            // steps with an element and more molecules cannot be simplified to a single element
            let validStep (s: string) : bool = not (s.Contains('e') && s.Length > 1)
            let isElement (s: string) : bool = s = "e"

            let nextReplacement (s: string) =
                let m =
                    this.sortedReplacements
                    |> Seq.tryFind (fun (a, b) -> s.Contains(b))

                match m with
                | None -> None
                | Some (a, b) ->
                    let i = s.LastIndexOf(b)
                    Some(a, b, s.[0 .. (i - 1)] + a + s.[i + (b.Length) ..])

            let generator (lastString: string, lastDepth) =
                // printfn "depth '%d'" lastDepth

                match nextReplacement lastString with
                | Some (a, b, p) -> Some((a, b, p), (p, lastDepth + 1)) // nothing more to do
                | None -> None

            (molecule, 0) |> Seq.unfold generator
        // |> mapRenderEach

        member this.cheapestConstructionSteps(molecule: string) =

            let elements =
                this.sortedReplacements
                |> Array.filter (fun (a, b) -> a = "e")
                |> Array.map fst

            let generator (lastPossibilities: Set<string>, lastDepth: int) =
                match lastPossibilities.Contains(molecule) with
                | true -> None // done
                | false ->
                    // printfn "molecule not yet found at depth %d (%d possibilities)" lastDepth lastPossibilities.Count
                    Some(lastPossibilities, (lastPossibilities |> generateAllFabrications, lastDepth + 1))

            (elements |> Set.ofSeq, 0) |> Seq.unfold generator

    type InputLine =
        | Replacement of string * string
        | Calibration of string

    let fromInput (s: string) =
        let parts =
            s
            |> splitToTrimmedLines
            |> Seq.map (fun s ->
                // printfn "s: %s" s
                s)
            |> Seq.map (fun s ->
                match s with
                | Regex @"^(\w+) => (\w+)$" [ a; b ] -> Replacement(a, b)
                | Regex @"^(\w+)$" [ c ] -> Calibration(c)
                | _ -> raise (Exception(sprintf "unrecognized line '%s'" s)))

        let replacements =
            parts
            |> Seq.map (fun p ->
                match p with
                | Replacement (x, y) -> Some(x, y)
                | _ -> None)
            |> Seq.choose id
            |> Array.ofSeq

        let calibration =
            parts
            |> Seq.map (fun p ->
                match p with
                | Calibration (x) -> Some(x)
                | _ -> None)
            |> Seq.choose id
            |> Seq.head

        Machine(replacements, calibration)
