namespace AdventOfCode

module Day06 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.day06_Wait_For_It
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
Time:      7  15   30
Distance:  9  40  200
"""

    let day06input = """
Time:        38     94     79     70
Distance:   241   1549   1074   1091
"""
    [<Fact>]
    let ``2023 - Day 06 - part 1 - example`` () =
        let exampleRecord = exampleInput |> parser.parseInput
            
        exampleRecord |> should equal {
            races = [|
                { t =  7; d =   9 }
                { t = 15; d =  40 }
                { t = 30; d = 200 }
            |]
        }

        exampleRecord.races
        |> Array.head
        |> fun r -> r.allStartingOptions
        |> Array.ofSeq
        |> should equal [|
                { t = 0; d =  0 }
                { t = 1; d =  6 }
                { t = 2; d = 10 }
                { t = 3; d = 12 }
                { t = 4; d = 12 }
                { t = 5; d = 10 }
                { t = 6; d =  6 }
                { t = 7; d =  0 }
            |]
        
        exampleRecord.races
        |> Array.head
        |> fun r -> r.winningRaces
        |> Array.ofSeq
        |> should equal [|
                { t = 2; d = 10 }
                { t = 3; d = 12 }
                { t = 4; d = 12 }
                { t = 5; d = 10 }
            |]
        
        exampleRecord.part1NumberOfWaysToWin
        |> Array.ofSeq
        |> should equal [|
            4
            8
            9
        |]
        
        exampleRecord.part1ProductOfNumberOfWaysToWin
        |> should equal 288

    // [<Fact>]
    let ``2023 - Day 06 - part 1`` () =
        let raceRecord =
            day06input
            |> parser.parseInput
        
        raceRecord.part1ProductOfNumberOfWaysToWin
        // |> printfn "2023 - Day 06 - Part 1: %A"
        |> should equal 1083852

    // [<Fact>]
    let ``2023 - Day 06 - part 2 - example`` () =
        let r =
            exampleInput
            |> parser.part2MustParse
            
        r |> should equal { t = 71530L; d = 940200L }
        r.winningRaces |> Seq.length |> should equal 71503

    // [<Fact>]
    let ``2023 - Day 06 - part 2`` () =
        day06input
        |> parser.part2MustParse
        |> (fun r -> r.winningRaces |> Seq.length)
        // |> printfn "2023 - Day 06 - Part 2: %A"
        |> should equal 23501589
