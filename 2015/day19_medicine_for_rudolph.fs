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
        member this.calibrationSet =
            replacements
            |> Array.collect (fun (a, b) ->
                let l = a.Length

                calibrationString
                |> Seq.windowed l
                |> Seq.mapi (fun i s -> (s |> String, i))
                |> Seq.filter (fun (s, _) -> s = a)
                // |> Seq.map (fun s ->
                //     printfn "> %A" s
                //     s)
                |> Seq.map snd
                |> Seq.map (fun i ->
                    calibrationString.[0 .. (i - 1)]
                    + b
                    + calibrationString.[(i + l) ..])
                // |> Seq.map (fun s ->
                //     printfn "> %A" s
                //     s)
                |> Array.ofSeq)
            |> Set.ofSeq

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
