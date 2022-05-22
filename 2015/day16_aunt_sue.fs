namespace AdventOfCode

module AuntSue =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    type SueProfile =
        { number: int
          children: int option
          cats: int option
          samoyeds: int option
          pomeranians: int option
          akitas: int option
          vizslas: int option
          goldfish: int option
          trees: int option
          cars: int option
          perfumes: int option }
        static member fromInput(n: int) =
            { number = n
              children = None
              cats = None
              samoyeds = None
              pomeranians = None
              akitas = None
              vizslas = None
              goldfish = None
              trees = None
              cars = None
              perfumes = None }

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Seq.map (fun ss ->
            match ss with
            | Regex @"^Sue (\d+): (.+)" [ number; rest ] ->
                let deets =
                    rest.Split(",")
                    |> Array.map (fun s ->
                        s.Trim().Split(": ")
                        |> fun parts -> (parts.[0], parts.[1] |> int))

                let folder (sue: SueProfile) (aspect: string, n: int) =
                    match aspect with
                    | "children" -> { sue with children = Some(n) }
                    | "cats" -> { sue with cats = Some(n) }
                    | "samoyeds" -> { sue with samoyeds = Some(n) }
                    | "pomeranians" -> { sue with pomeranians = Some(n) }
                    | "akitas" -> { sue with akitas = Some(n) }
                    | "vizslas" -> { sue with vizslas = Some(n) }
                    | "goldfish" -> { sue with goldfish = Some(n) }
                    | "trees" -> { sue with trees = Some(n) }
                    | "cars" -> { sue with cars = Some(n) }
                    | "perfumes" -> { sue with perfumes = Some(n) }
                    | _ -> raise (System.Exception(sprintf "unrecognized aspect: '%s'" aspect))

                deets
                |> Array.fold folder (SueProfile.fromInput (number |> int))

            | _ -> raise (System.Exception(sprintf "could not parse: '%s'" ss)))

        |> Array.ofSeq


    let findSueMatching (expected: SueProfile) (allKnown: SueProfile []) =
        let isKnownMatchOrTrue (s: SueProfile) (aspect: string) (a: int option) (e: int option) =
            match e with
            | None -> true // don't know expected value; could be a match
            | Some (ev) ->
                match a with
                | None -> true // don't remember this Sue's value; could be a match
                | Some (n) ->
                    match n = ev with // know both; a match if the values match
                    | true -> true
                    | false ->
                        // printfn "%A failed to match on %s\n" s aspect
                        false

        allKnown
        |> Array.filter (fun sue ->
            // (sue.children.IsSome && sue.children.Value = expected.children.Value) || (sue.cats.IsSome && sue)
            isKnownMatchOrTrue sue "children" (sue.children) (expected.children)
            && isKnownMatchOrTrue sue "cats" (sue.cats) (expected.cats)
            && isKnownMatchOrTrue sue "samoyeds" (sue.samoyeds) (expected.samoyeds)
            && isKnownMatchOrTrue sue "pomeranians" (sue.pomeranians) (expected.pomeranians)
            && isKnownMatchOrTrue sue "akitas" (sue.akitas) (expected.akitas)
            && isKnownMatchOrTrue sue "vizslas" (sue.vizslas) (expected.vizslas)
            && isKnownMatchOrTrue sue "goldfish" (sue.goldfish) (expected.goldfish)
            && isKnownMatchOrTrue sue "trees" (sue.trees) (expected.trees)
            && isKnownMatchOrTrue sue "cars" (sue.cars) (expected.cars)
            && isKnownMatchOrTrue sue "perfumes" (sue.perfumes) (expected.perfumes))

    let findSueMatching2 (expected: SueProfile) (allKnown: SueProfile []) =
        let isKnownMatchOrTrue (s: SueProfile) (aspect: string) (a: int option) (e: int option) =
            match e with
            | None -> true // don't know expected value; could be a match
            | Some (ev) ->
                match a with
                | None -> true // don't remember this Sue's value; could be a match
                | Some (n) -> n = ev // know both; a match if the values match

        let isKnownLowerThanOrTrue (s: SueProfile) (aspect: string) (a: int option) (e: int option) =
            match e with
            | None -> true // don't know expected value; could be a match
            | Some (ev) ->
                match a with
                | None -> true // don't remember this Sue's value; could be a match
                | Some (n) -> n < ev // know both; a match if this value is less than expected

        let isKnownGreaterThanOrTrue (s: SueProfile) (aspect: string) (a: int option) (e: int option) =
            match e with
            | None -> true // don't know expected value; could be a match
            | Some (ev) ->
                match a with
                | None -> true // don't remember this Sue's value; could be a match
                | Some (n) -> n > ev // know both; a match if this value is greater than expected

        allKnown
        |> Array.filter (fun sue ->
            // (sue.children.IsSome && sue.children.Value = expected.children.Value) || (sue.cats.IsSome && sue)
            isKnownMatchOrTrue sue "children" (sue.children) (expected.children)
            && isKnownGreaterThanOrTrue sue "cats" (sue.cats) (expected.cats)
            && isKnownMatchOrTrue sue "samoyeds" (sue.samoyeds) (expected.samoyeds)
            && isKnownLowerThanOrTrue sue "pomeranians" (sue.pomeranians) (expected.pomeranians)
            && isKnownMatchOrTrue sue "akitas" (sue.akitas) (expected.akitas)
            && isKnownMatchOrTrue sue "vizslas" (sue.vizslas) (expected.vizslas)
            && isKnownLowerThanOrTrue sue "goldfish" (sue.goldfish) (expected.goldfish)
            && isKnownGreaterThanOrTrue sue "trees" (sue.trees) (expected.trees)
            && isKnownMatchOrTrue sue "cars" (sue.cars) (expected.cars)
            && isKnownMatchOrTrue sue "perfumes" (sue.perfumes) (expected.perfumes))
