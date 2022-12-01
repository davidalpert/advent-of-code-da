namespace AdventOfCode

module Day01 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.CalorieCounting
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

    let puzzleInput =
        """
"""

    [<Fact>]
    let ``2015 - Day 01 - example`` () =
        exampleInput
        |> fromInput
        |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2015 - Day 01 - part 1`` () =
        puzzleInput
        |> fromInput
        |> Array.length
        |> should equal 0
