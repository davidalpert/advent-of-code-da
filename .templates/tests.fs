namespace AdventOfCode

module DayDAY_NUMBER =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.MODULE_NAME
    open AdventOfCode.DayDAY_NUMBERInput
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
"""

    [<Fact>]
    let ``YEAR - Day DAY_NUMBER - part 1 - example`` () =
        exampleInput
        |> parser.parseInput
        |> should equal null

    // [<Fact>]
    let ``YEAR - Day DAY_NUMBER - part 1`` () =
        dayDAY_NUMBERinput
        // |> fromInput
        // |> Array.length
        |> printfn "YEAR - Day DAY_NUMBER - Part 1: %A"

    // [<Fact>]
    let ``YEAR - Day DAY_NUMBER - part 2 - example`` () =
        exampleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``YEAR - Day DAY_NUMBER - part 2`` () =
        dayDAY_NUMBERinput
        // |> fromInput
        // |> Array.length
        |> printfn "YEAR - Day DAY_NUMBER - Part 2: %A"
