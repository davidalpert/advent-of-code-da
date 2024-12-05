namespace AdventOfCode

module Day04 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.day04_Ceres_Search
    open AdventOfCode.Day04Input
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"""

    [<Fact>]
    let ``2024 - Day 04 - part 1 - example`` () =
        exampleInput
        |> fromInput
        |> part1.findInstances
        |> Array.length
        |> should equal 18

    [<Fact>]
    let ``2024 - Day 04 - part 1`` () =
        day04input
        |> fromInput
        |> part1.findInstances
        |> Array.length
        |> should equal 2406
        // |> printfn "2024 - Day 04 - Part 1: %A"

    // [<Fact>]
    let ``2024 - Day 04 - part 2 - example`` () =
        exampleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2024 - Day 04 - part 2`` () =
        day04input
        // |> fromInput
        // |> Array.length
        |> printfn "2024 - Day 04 - Part 2: %A"
