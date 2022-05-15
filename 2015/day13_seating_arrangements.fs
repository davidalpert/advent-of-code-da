namespace AdventOfCode

module SeatingArrangements =

    open AdventOfCode.utils

    let parseHappinessRelationship (s: string) =
        match s with
        | Regex @"(\w+) would gain (\d+) happiness units by sitting next to (\w+)" [ x; d; y ] -> ((x, y), d |> int)
        | Regex @"(\w+) would lose (\d+) happiness units by sitting next to (\w+)" [ x; d; y ] ->
            ((x, y), (d |> int) * -1)
        | _ -> raise (System.Exception(sprintf "could not parse: '%s'" s))

    let parseInput (s: string) =
        s.Trim().Split("\n")
        |> Array.map (fun ss -> ss.Trim())
        |> Array.map parseHappinessRelationship

    let toImpactMap (s: string) = s |> parseInput |> Map.ofSeq

    let findHappiestAggangement (includeApatheticSelf: bool) (s: string) =
        let mutable relationships = s |> parseInput

        let findUniqueGuests (rr: ((string * string) * int) []) =
            rr
            |> Array.collect (fun ((x, y), _) -> [| x; y |])
            |> Array.distinct

        let mutable knights = relationships |> findUniqueGuests

        if includeApatheticSelf then
            let myRelationships =
                knights
                |> Array.collect (fun k ->
                    [| (("myself", k), 0)
                       ((k, "myself"), 0) |])

            relationships <-
                [| relationships; myRelationships |]
                |> Array.concat

            knights <- relationships |> findUniqueGuests

        let impactMap = relationships |> Map.ofSeq

        let possibilities =
            (permuteAll (knights |> List.ofArray))
            |> Seq.map (fun l -> l |> Array.ofList)

        possibilities
        |> Seq.map (fun arrangement ->
            let size = arrangement.Length

            let happinessDeltaPerPerson =
                arrangement
                |> Array.mapi (fun i name ->
                    let leftI =
                        (match i with
                         | 0 -> size
                         | _ -> i)
                        - 1

                    let rightI = (i + 1) % size

                    let leftName = arrangement.[leftI]
                    let rightName = arrangement.[rightI]

                    let leftImpact = impactMap.[(name, leftName)]
                    let rightImpact = impactMap.[(name, rightName)]

                    // printfn
                    //     "%s <(%d)< %s [%d] >(%d)> %s"
                    //     leftName
                    //     leftImpact
                    //     name
                    //     (leftImpact + rightImpact)
                    //     rightImpact
                    //     rightName

                    leftImpact + rightImpact)

            Array.zip arrangement happinessDeltaPerPerson)

        |> Seq.map (fun arrangement ->
            let (names, impacts) = arrangement |> Array.unzip
            (names, impacts |> Array.sum))
        |> Seq.sortByDescending (fun (_, impact) -> impact)
        // |> Seq.map (fun r ->
        //     printfn "%A" r
        //     r)
        |> Seq.maxBy (fun (_, impact) -> impact)
