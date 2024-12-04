namespace AdventOfCode

module Day02 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.day02_Red_Nosed_Reports
    open AdventOfCode.Day02Input
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"""

    [<Fact>]
    let ``2024 - Day 02 - part 1 - example`` () =
        exampleInput
        |> fromInput
        |> countSafe part1ReportIsSafe
        |> should equal 2

    [<Fact>]
    let ``2024 - Day 02 - part 1`` () =
        day02input
        |> fromInput
        |> countSafe part1ReportIsSafe
        |> should equal 606
        // |> printfn "2024 - Day 02 - Part 1: %A"

    [<Fact>]
    let ``2024 - Day 02 - part 2 - example`` () =
        exampleInput
        |> fromInput
        |> countSafe part2ReportIsSafe 
        |> should equal 4

    [<Fact>]
    let ``2024 - Day 02 - part 2`` () =
        day02input
        |> fromInput
        |> countSafe part2ReportIsSafe 
        |> should equal 644
        // |> printfn "2024 - Day 02 - Part 2: %A"
