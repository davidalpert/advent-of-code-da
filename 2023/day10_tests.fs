namespace AdventOfCode

open System.Security
open FSharpAux

module Day10 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.utils
    open AdventOfCode.day10_Pipe_Maze
    open Xunit
    open FsUnit.Xunit

    let example1input = """
.....
.....
.S-7.
.|.|.
.L-J.
.....
"""

    let example2input = """
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
"""

    let example3input = """
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
"""

    let example4input = """
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
"""

    [<Fact>]
    let ``2023 - Day 10 - part 1 - example`` () =
        let a = example1input |> splitTo2DArray parser.char2tile
        Array2D.length1 a |> should equal 6
        Array2D.length2 a |> should equal 5
        
        let map = example1input |> parser.parseInput
        
        map.startingPosition |> should equal { x = 1; y = 2 }
        
        map[map.startingPosition] |> should equal StartingPosition
        
        map.possibleConnectionPoints map.startingPosition
        |> Seq.map (fun p -> p,map[p]) |> List.ofSeq
        |> should equal [
            { x = 0; y = 2 },Ground         // west (.)
            { x = 1; y = 1 },Ground         // north (.)
            { x = 1; y = 3 },VerticalPipe   // south (|)
            { x = 2; y = 2 },HorizontalPipe // east (-)
        ]
        
        map.positionsConnectedTo map.startingPosition
        |> Seq.map (fun p -> p,map[p]) |> List.ofSeq
        |> should equal [
            { x = 1; y = 3 },VerticalPipe   // south (|)
            { x = 2; y = 2 },HorizontalPipe // east (-)
        ]
        
        map.walkHalfway
        |> Seq.map (fun (a,b) -> $"%s{a.ToString()}, %s{b.ToString()}") |> joinBy "\n"
        |> should equal ("""
(1,2), (1,2)
(1,3), (2,2)
(1,4), (3,2)
(2,4), (3,3)
(3,4), (3,4)
""".Trim())
        
        map.numberOfStepsToFarthestPoint
        |> should equal 4
        
        (example2input |> parser.parseInput).numberOfStepsToFarthestPoint
        |> should equal 4
        
        (example3input |> parser.parseInput).numberOfStepsToFarthestPoint
        |> should equal 8
        
        (example4input |> parser.parseInput).numberOfStepsToFarthestPoint
        |> should equal 8

    // [<Fact>]
    let ``2023 - Day 10 - part 1`` () =
        let map = day10input |> parser.parseInput
        map.numberOfStepsToFarthestPoint
        // |> printfn "2023 - Day 10 - Part 1: %A"
        |> should equal 6897

    // [<Fact>]
    let ``2023 - Day 10 - part 2 - example`` () =
        example1input
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2023 - Day 10 - part 2`` () =
        day10input
        // |> fromInput
        // |> Array.length
        |> printfn "2023 - Day 10 - Part 2: %A"
