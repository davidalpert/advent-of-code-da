namespace AdventOfCode

open ApprovalTests

module Day03 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.day03_Gear_Ratios
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

    let puzzleInput = ""

    [<Fact>]
    let ``2023 - Day 03 - part 1 - example`` () =
        let schematic = exampleInput.Trim() |> parser.parseInput
        
        schematic.numbers |> Array.length |> should equal 10
        schematic.symbols |> Array.length |> should equal 6
        
        schematic
        |> sprintf "%A"
        |> Approvals.Verify
        
        schematic.numbers[0]
        |> sprintf "%A"
        |> should equal """{ p = { y = 1L
        x = 1L }
  len = 3L
  v = 467 }"""
  
        schematic.numbers[0].adjacentPositions
        |> should equal [|
             { y = 0L; x = 0L }; { y = 0L; x = 1L }; { y = 0L; x = 2L }; { y = 0L; x = 3L }; { y = 0L; x = 4L };
             { y = 1L; x = 0L }; (*       4                    6                   7      *) { y = 1L; x = 4L };
             { y = 2L; x = 0L }; { y = 2L; x = 1L }; { y = 2L; x = 2L }; { y = 2L; x = 3L }; { y = 2L; x = 4L }; 
        |]

        schematic.numbers[2]
        |> sprintf "%A"
        |> should equal """{ p = { y = 3L
        x = 3L }
  len = 2L
  v = 35 }"""
        
        schematic.partNumbers |> Array.length |> should equal 8
        
        schematic.sumOfPartNumbers
        |> should equal 4361

    [<Fact>]
    let ``2023 - Day 03 - part 1`` () =
        day03input
        |> parser.parseInput
        |> sumOfPartNumbers
        // |> should equal 521927 // too low
        |> printfn "2023 - Day 03 - Part 1: %A"

    // [<Fact>]
    let ``2023 - Day 03 - part 2 - example`` () =
        exampleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2023 - Day 03 - part 2`` () =
        puzzleInput
        // |> fromInput
        // |> Array.length
        |> printfn "2023 - Day 03 - Part 2: %A"
