namespace AdventOfCode

open System.Security
open FSharpAux

module Day10 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.utils
    open AdventOfCode.Grid2D
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

    let part2example1input = """
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
"""

    let part2example1Expected = ("""
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|II|.|II|.
.L--J.L--J.
...........
""".Trim())

    let part2example2input = """
..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........
"""

    let part2example2expected = ("""
..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|II||II|.
.L--JL--J.
..........
""".Trim())

    let part2example3input = """
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
"""

    let part2example3expected = ("""
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJIL-7..
L--J.L7IIILJS7F-7L7.
....F-JIIF7FJ|L7L7L7
....L7IF7||L7|IL7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
""".Trim())

    let part2example4input = """
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
"""

    let part2example4expected = ("""
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJIF7FJ-
L---JF-JLJIIIIFJLJJ7
|F|F-JF---7IIIL7L|7|
|FFJF7L7F-JF7IIL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
""".Trim())

    // [<Fact>]
    let ``2023 - Day 10 - part 2 - example`` () =
        let map = part2example1input |> parser.parseInput
        // |> fromInput
        // |> Array.length
        // map.walkHalfway
        // |> Seq.map (fun (a,b) -> $"%s{a.ToString()}, %s{b.ToString()}") |> joinBy "\n"
        // |> should equal null

        map.walkToEdge westFrom map.startingPosition
        |> Seq.map (fun p -> p.ToString()) |> joinBy "\n"
        |> should equal ("""
(0,1)
(1,1)
""".Trim())

        map.walkToEdge eastFrom map.startingPosition
        |> Seq.map (fun p -> p.ToString()) |> joinBy "\n"
        |> should equal ("""
(1,1)
(2,1)
(3,1)
(4,1)
(5,1)
(6,1)
(7,1)
(8,1)
(9,1)
(10,1)
""".Trim())

        map.ToString()
        |> should equal ("""
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........                         
""".Trim())
        
        map.fillFrom (Pos.fromTuple(2,6))
        |> snd |> should equal false

        map.ToString()
        |> should equal ("""
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|II|.|..|.
.L--J.L--J.
...........
""".Trim())

        map.fillFrom (Pos.fromTuple(0,0))
        |> snd |> should equal true

        map.ToString()

        |> should equal ("""
OOOOOOOOOOO
OS-------7O
O|F-----7|O
O||OOOOO||O
O||OOOOO||O
O|L-7OF-J|O
O|II|O|..|O
OL--JOL--JO
OOOOOOOOOOO
""".Trim())
        // map.loopPositions
        // |> Seq.map (fun p -> p.ToString())
        // |> joinBy "\n"
        // |> should equal null

    // [<Fact>]
    let ``2023 - Day 10 - part 2`` () =
        day10input
        // |> fromInput
        // |> Array.length
        |> printfn "2023 - Day 10 - Part 2: %A"
