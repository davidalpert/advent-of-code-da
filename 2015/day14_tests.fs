namespace AdventOfCode.Tests

module Day14 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.ReindeerOlympics
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
"""

    let puzzleInput =
        """
Vixen can fly 19 km/s for 7 seconds, but then must rest for 124 seconds.
Rudolph can fly 3 km/s for 15 seconds, but then must rest for 28 seconds.
Donner can fly 19 km/s for 9 seconds, but then must rest for 164 seconds.
Blitzen can fly 19 km/s for 9 seconds, but then must rest for 158 seconds.
Comet can fly 13 km/s for 7 seconds, but then must rest for 82 seconds.
Cupid can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
Dancer can fly 3 km/s for 16 seconds, but then must rest for 37 seconds.
Prancer can fly 25 km/s for 6 seconds, but then must rest for 143 seconds.
"""

    [<Theory>]
    [<InlineData(1, 0, true, 14)>]
    [<InlineData(10, 0, true, 140)>]
    [<InlineData(11, 0, false, 140)>]
    [<InlineData(136, 0, false, 140)>]
    [<InlineData(137, 1, false, 140)>]
    [<InlineData(138, 1, true, 154)>]
    let ``Day 14 - part 1 - Comet's distanceAfterNSeconds`` (n, cyclesCompleted, moving, expectedDistance) =
        let s =
            exampleInput
            |> splitToTrimmedLines
            |> Seq.head
            |> parseOneStat

        // Comet's cycle duration is flight duration + rest duration
        s.cycleDuration |> int |> should equal 137

        s.distancePerCycle
        |> should equal ((140 |> float) * 1.0<kilometer>)

        s.cyclesCompletedAfterNSeconds n
        |> should equal cyclesCompleted

        s.distanceAfterNSeconds n
        |> should equal (expectedDistance * 1.0<kilometer>)

    [<Theory>]
    [<InlineData(1, 14, 16)>]
    [<InlineData(10, 140, 160)>]
    [<InlineData(11, 140, 176)>]
    [<InlineData(12, 140, 176)>]
    [<InlineData(137, 140, 176)>]
    [<InlineData(1000, 1120, 1056)>]
    let ``Day14 - part 1 - competeForNSeconds`` (n, cometDistance, dancerDistance) =
        let stats = exampleInput |> parseStats

        let m = stats |> competeForNSeconds n

        m.["Comet"]
        |> should equal (cometDistance * 1.0<kilometer>)

        m.["Dancer"]
        |> should equal (dancerDistance * 1.0<kilometer>)

    [<Fact>]
    let ``Day14 - part 1 - calculation`` () =
        puzzleInput
        |> parseStats
        |> competeForNSeconds (2503 * 1<second>)
        |> Seq.maxBy (fun pair -> pair.Value)
        |> (fun pair -> pair.Value)
        |> should equal ((2660 |> float) * 1.0<kilometer>)
