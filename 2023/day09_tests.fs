namespace AdventOfCode

module Day09 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.day09_Mirage_Maintenance
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""

    [<Fact>]
    let ``2023 - Day 09 - part 1 - example`` () =
        let report = exampleInput |> parser.parseInput
        
        report |> should equal {
            historicalValuesByIndex =
               Map([
                   (0, [|0; 3; 6; 9; 12; 15|])
                   (1, [|1; 3; 6; 10; 15; 21|])
                   (2, [|10; 13; 16; 21; 30; 45|])
               ])
        }
        
        report.nextValueForIndex 0 |> should equal 18
        report.nextValueForIndex 1 |> should equal 28
        report.nextValueForIndex 2 |> should equal 68
        
        report.sumOfNextValues |> should equal 114

    [<Fact>]
    let ``2023 - Day 09 - part 1`` () =
        let report = day09input |> parser.parseInput
        report.sumOfNextValues
        // |> printfn "2023 - Day 09 - Part 1: %A"
        |> should equal 1980437560

    // [<Fact>]
    let ``2023 - Day 09 - part 2 - example`` () =
        exampleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2023 - Day 09 - part 2`` () =
        day09input
        // |> fromInput
        // |> Array.length
        |> printfn "2023 - Day 09 - Part 2: %A"
