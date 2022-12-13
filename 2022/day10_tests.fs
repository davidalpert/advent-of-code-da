namespace AdventOfCode

module Day10 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.CathodeRayTube
    open Xunit
    open FsUnit.Xunit

    let smallExampleInput = """
noop
addx 3
addx -5
"""

    let largerExampleInput =
        """
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"""

    let puzzleInput =
        """
noop
noop
addx 5
addx 21
addx -16
noop
addx 1
noop
noop
addx 4
addx 1
addx 4
addx 1
noop
addx 4
addx -9
noop
addx 19
addx -5
noop
noop
addx 5
addx 1
addx -38
addx 5
addx -2
addx 2
noop
noop
addx 7
addx 9
addx 20
addx -3
addx -18
addx 2
addx 5
noop
noop
addx -2
noop
noop
addx 7
addx 3
addx -2
addx 2
addx -28
addx -7
addx 5
noop
addx 2
addx 32
addx -27
noop
noop
noop
noop
noop
addx 7
noop
addx 22
addx -19
noop
addx 5
noop
addx -7
addx 17
addx -7
noop
addx -20
addx 27
noop
addx -16
addx -20
addx 1
noop
addx 3
addx 15
addx -8
addx -2
addx -6
addx 14
addx 4
noop
noop
addx -17
addx 22
noop
addx 5
noop
noop
noop
addx 2
noop
addx 3
addx -32
addx -5
noop
addx 4
addx 3
addx -2
addx 34
addx -27
addx 5
addx 16
addx -18
addx 7
noop
addx -2
addx -1
addx 8
addx 14
addx -9
noop
addx -15
addx 16
addx 2
addx -35
noop
noop
noop
noop
addx 3
addx 4
noop
addx 1
addx 4
addx 1
noop
addx 4
addx 2
addx 3
addx -5
addx 19
addx -9
addx 2
addx 4
noop
noop
noop
noop
addx 3
addx 2
noop
noop
noop
"""

    // [<Fact>]
    let ``2022 - Day 10 - part 1 - small example - creating the sequence`` () =
        let expected = [
                "cycle = 1; value = 1"; // noop (starts and ends)
                "cycle = 2; value = 1"; // addx 3 (starts)
                "cycle = 3; value = 1"; // addx 3 (ends and applies to next cycle)
                "cycle = 4; value = 4"; // addx -5 (starts)
                "cycle = 5; value = 4"; // addx -5 (ends and applies to next cycle)
                "cycle = 6; value = -1"; //
            ]
        
        let state =
            smallExampleInput
            |> parser.parseInstructions
            |> apply
            |> Seq.map (fun c ->
                    sprintf $"cycle = %d{c.number}; value = %d{c.valueDuring}"
                )
            |> Seq.take expected.Length
            |> List.ofSeq
            
        System.String.Join("\n", state)
        |> should equal (System.String.Join("\n", expected))

    [<Fact>]
    let ``2022 - Day 10 - part 1 - small example - selected cycles`` () =
        let expected = [
                "cycle = 20; value = 21; signalStrength = 420";
                "cycle = 60; value = 19; signalStrength = 1140";
                "cycle = 100; value = 18; signalStrength = 1800";
                "cycle = 140; value = 21; signalStrength = 2940";
                "cycle = 180; value = 16; signalStrength = 2880";
                "cycle = 220; value = 18; signalStrength = 3960";
            ]
        
        let samples =
            largerExampleInput
            |> parser.parseInstructions
            |> apply
            |> Seq.skip 19 // so that next is cycle 20
            |> Seq.chunkBySize 40
            |> Seq.take 6
            |> Array.ofSeq
            |> Seq.map (fun cc -> cc[0])
            
        let asString =
            samples
            |> Seq.map (fun c ->
                    sprintf $"cycle = %d{c.number}; value = %d{c.valueDuring}; signalStrength = %d{signalStrength c}"
                )
            |> List.ofSeq
            
        System.String.Join("\n", asString)
        |> should equal (System.String.Join("\n", expected))
        
        samples
        |> Seq.sumBy signalStrength
        |> should equal 13140

    [<Fact>]
    let ``2022 - Day 10 - part 1 - small example`` () =
        largerExampleInput
        |> part1_whatIsTheSumOfNSampledSignalStrengths 6
        |> should equal 13140
        
    [<Fact>]
    let ``2022 - Day 10 - part 1`` () =
        puzzleInput
        |> part1_whatIsTheSumOfNSampledSignalStrengths 6
        |> printfn "2022 - Day 10 - Part 1: %A"

    // [<Fact>]
    let ``2022 - Day 10 - part 2 - example`` () =
        smallExampleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2022 - Day 10 - part 2`` () =
        puzzleInput
        // |> fromInput
        // |> Array.length
        |> printfn "2022 - Day 10 - Part 2: %A"
