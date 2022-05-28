namespace AdventOfCode

module Day20 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.InfiniteElvesandInfiniteHouses
    open Xunit
    open FsUnit.Xunit

    let puzzleInput = 36000000

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
        divisorsAsSeq num
        |> Seq.length
        |> should equal numFactors

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
    [<InlineData(831600, 0, 36902400)>] // <- part 1 answer (after finding it in the following test)
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
    [<InlineData(36000000, 831600)>] // <- part 1 answer; takes 8s :tada:
    let ``2015 - Day 20 - part 1`` (minNumberPresents, expected) =
        houseNumbersWhichGetAtLeastNPresents minNumberPresents
        |> Seq.head
        |> should equal expected

    [<Theory>]
    [<InlineData(36000000, 884520)>] // <- part 2 answer; takes ~8s
    let ``2015 - Day 20 - part 2`` (minNumberPresents, expected) =
        houseNumbersWhichGetAtLeastNPresentsWithElvesStoppingAfter50Houses minNumberPresents
        // |> Seq.head
        |> should equal expected
