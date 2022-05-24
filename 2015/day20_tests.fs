namespace AdventOfCode

module Day20 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.InfiniteElvesandInfiniteHouses
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
"""

    let puzzleInput =
        """
"""

    [<Theory>]
    [<InlineData(1, 1)>]
    [<InlineData(2, 2)>]
    [<InlineData(3, 2)>]
    [<InlineData(4, 3)>]
    [<InlineData(5, 2)>]
    [<InlineData(6, 4)>]
    [<InlineData(7, 2)>]
    [<InlineData(8, 4)>]
    [<InlineData(9, 3)>]
    [<InlineData(100, 9)>]
    let ``2015 - Day 20 - divisorsOf`` (num, numFactors) =
        divisorsOf num
        |> Seq.length
        |> should equal numFactors

    // [<Theory>] // doesn't work; these were based on hacking DivMap
    [<InlineData(1, "1", 1)>]
    [<InlineData(2, "11", 3)>]
    [<InlineData(3, "101", 4)>]
    [<InlineData(4, "1101", 7)>]
    [<InlineData(5, "10001", 6)>]
    let ``2015 - Day 20 - DivMap`` (n, expectedBits, expectedSum) =
        let bitsToMap (bm: string) =
            ("0" + bm) // pad with initial 0 for the 0th place
            |> Seq.map (fun c ->
                match c with
                | '1' -> true
                | _ -> false)
            |> Array.ofSeq

        DivMap n |> should equal (bitsToMap expectedBits)
        SumDivMap n |> should equal expectedSum

    [<Theory>]
    [<InlineData(1, 1, 10)>]
    [<InlineData(2, 2, 30)>]
    [<InlineData(3, 2, 40)>]
    [<InlineData(4, 3, 70)>]
    [<InlineData(5, 2, 60)>]
    [<InlineData(6, 4, 120)>]
    [<InlineData(7, 2, 80)>]
    [<InlineData(8, 4, 150)>]
    [<InlineData(9, 3, 130)>]
    let ``2015 - Day 20 - numPresentsDeliveredToHouseN`` (house, _, numPresentsDelivered) =
        house
        |> numPresentsDeliveredToHouseN
        |> should equal numPresentsDelivered

    [<Theory>]
    [<InlineData(10, 1)>]
    [<InlineData(40, 3)>]
    [<InlineData(60, 4)>]
    [<InlineData(80, 6)>]
    [<InlineData(100, 6)>]
    // [<InlineData(36000000, 831600)>] // <- part 1 answer; takes 18m
    let ``2015 - Day 20 - part 1`` (minNumberPresents, expected) =
        houseNumbersWhichGetAtLeastNPresents minNumberPresents
        |> Seq.head
        |> should equal expected
