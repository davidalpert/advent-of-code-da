namespace AdventOfCode

module Day01 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.day01_Historian_Hysteria
    open AdventOfCode.Day01Input
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
3   4
4   3
2   5
1   3
3   9
3   3
"""

    [<Fact>]
    let ``2024 - Day 01 - part 1 - example`` () =
        exampleInput
        |> calculateDistanceBetweenTwoLists
        |> should equal 11

    [<Fact>]
    let ``2024 - Day 01 - part 1`` () =
        day01input
        |> calculateDistanceBetweenTwoLists
        |> should equal 1197984
        // |> printfn "2024 - Day 01 - Part 1: %A"

    [<Fact>]
    let ``2024 - Day 01 - part 2 - example`` () =
        exampleInput
        |> calculateSimilarityScore
        |> should equal 31

    [<Fact>]
    let ``2024 - Day 01 - part 2`` () =
        day01input
        |> calculateSimilarityScore
        |> should equal 23387399
        // |> printfn "2024 - Day 01 - Part 2: %A"
