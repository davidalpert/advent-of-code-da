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

    [<Fact>]
    let ``2023 - Day 05 - part 2 - expandedSeedList`` () =
        exampleInput
        |> parser.parseInput
        |> (fun a -> a.expandedSeedList)
        |> Array.map (fun s -> s.number)
        |> should equal [|
            79L; 80L; 81L; 82L; 83L; 84L; 85L; 86L; 87L; 88L; 89L; 90L; 91L; 92L
            55L; 56L; 57L; 58L; 59L; 60L; 61L; 62L; 63L; 64L; 65L; 66L; 67L
        |]

    [<Fact>]
    let ``2023 - Day 05 - part 2 - example`` () =
        exampleInput
        |> parser.parseInput
        |> (fun a -> a.part2SeedWithLowestLocation)
        |> should equal {
            number = 82; soil = 84; fertilizer = 84; water = 84; light = 77; temperature = 45; humidity = 46; location = 46;
        }

    // [<Fact>]
    // TODO: resolve performance bottleneck; part2 calculations run too long
    let ``2023 - Day 05 - part 2`` () =
        day05input
        |> parser.parseInput
        |> (fun a -> a.part2SeedWithLowestLocation)
        |> printfn "2023 - Day 05 - Part 2: %A"
