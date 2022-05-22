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
    let ``2015 - Day DAY_NUMBER - example`` () =
        exampleInput
        |> fromInput
        |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2015 - Day DAY_NUMBER - part 1`` () =
        puzzleInput
        |> fromInput
        |> Array.length
        |> should equal 0
