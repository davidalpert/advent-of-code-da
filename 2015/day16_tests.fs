namespace AdventOfCode

module Day16 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.AuntSue
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
Sue 1: cars: 9, akitas: 3, goldfish: 0
Sue 2: akitas: 9, children: 3, samoyeds: 9
"""

    let mfcsamProfile =
        """
children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
"""

    [<Fact>]
    let ``validate fromInput`` () =
        exampleInput
        |> fromInput
        |> Array.length
        |> should equal 2

    [<Fact>]
    let ``Day 16 - part 1 - calculate`` () =
        let mfcsam =
            { SueProfile.fromInput 0 with
                children = Some(3)
                cats = Some(7)
                samoyeds = Some(2)
                pomeranians = Some(3)
                akitas = Some(0)
                vizslas = Some(0)
                goldfish = Some(5)
                trees = Some(3)
                cars = Some(2)
                perfumes = Some(1) }

        let possibleMatches = day16input |> fromInput |> findSueMatching mfcsam
        //   |> should equal ""

        possibleMatches.Length |> should equal 1

        possibleMatches.[0].number |> should equal 373

    [<Fact>]
    let ``Day 16 - part 2 - calculate`` () =
        let mfcsam =
            { SueProfile.fromInput 0 with
                children = Some(3)
                cats = Some(7)
                samoyeds = Some(2)
                pomeranians = Some(3)
                akitas = Some(0)
                vizslas = Some(0)
                goldfish = Some(5)
                trees = Some(3)
                cars = Some(2)
                perfumes = Some(1) }

        let possibleMatches = day16input |> fromInput |> findSueMatching2 mfcsam
        //   |> should equal ""

        possibleMatches.Length |> should equal 1

        possibleMatches.[0].number |> should equal 260
