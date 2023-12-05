namespace AdventOfCode

module Day05 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.day05_If_You_Give_A_Seed_A_Fertilizer
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

    [<Fact>]
    let ``2023 - Day 05 - part 1 - example`` () =
        let locations =
            exampleInput
            |> parser.parseInput
            |> (fun a -> a.calculateLocations)
            
        locations
        |> should equal [|
            { number = 79L; soil = 81L; fertilizer = 81L; water = 81L; light = 74L; temperature = 78L; humidity = 78L; location = 82L; }
            { number = 14L; soil = 14L; fertilizer = 53L; water = 49L; light = 42L; temperature = 42L; humidity = 43L; location = 43L; }
            { number = 55L; soil = 57L; fertilizer = 57L; water = 53L; light = 46L; temperature = 82L; humidity = 82L; location = 86L; }
            { number = 13L; soil = 13L; fertilizer = 52L; water = 41L; light = 34L; temperature = 34L; humidity = 35L; location = 35L; }
        |]

    [<Fact>]
    let ``2023 - Day 05 - part 1`` () =
        day05input
        |> parser.parseInput
        |> (fun a -> a.lowestLocation)
        // |> printfn "2023 - Day 05 - Part 1: %A"
        |> should equal 107430936L

    // [<Fact>]
    let ``2023 - Day 05 - part 2 - example`` () =
        exampleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2023 - Day 05 - part 2`` () =
        day04input
        // |> fromInput
        // |> Array.length
        |> printfn "2023 - Day 05 - Part 2: %A"
