namespace AdventOfCode

module Day17 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.NoSuchThingAsTooMuch
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
20
15
10
5
5
"""

    let puzzleInput =
        """
11
30
47
31
32
36
3
1
5
3
32
36
15
11
46
26
28
1
19
3
"""

    [<Fact>]
    let ``2015 - Day 17 - example`` () =
        exampleInput
        |> fromInput
        |> differentWaysToStoreNLitres 25
        |> Array.length
        |> should equal 4

    [<Fact>]
    let ``2015 - Day 17 - part 1`` () =
        puzzleInput
        |> fromInput
        |> differentWaysToStoreNLitres 150
        |> Array.length
        |> should equal 4372
