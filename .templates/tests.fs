namespace AdventOfCode

module DayDAY_NUMBER =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.MODULE_NAME
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
"""

    let puzzleInput =
        """
"""

    [<Fact>]
    let ``YEAR - Day DAY_NUMBER - part 1 - example`` () =
        exampleInput
        |> fromInput
        |> Array.length
        |> should equal 1

    // [<Fact>]
    let ``YEAR - Day DAY_NUMBER - part 1`` () =
        puzzleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``YEAR - Day DAY_NUMBER - part 2 - example`` () =
        exampleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``YEAR - Day DAY_NUMBER - part 2`` () =
        puzzleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0
