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
        |> should equal 525181
        // |> printfn "2023 - Day 03 - Part 1: %A"

    (*
    In [the example above], there are two gears. The first is in the top left;
    it has part numbers 467 and 35, so its gear ratio is 16345. The second
    gear is in the lower right; its gear ratio is 451490.
    
    (The * adjacent to 617 is not a gear because it is only adjacent to
    one part number.) Adding up all of the gear ratios produces 467835.
    *)
    [<Fact>]
    let ``2023 - Day 03 - part 2 - example`` () =
        let schematic = exampleInput |> parser.parseInput
        
        schematic.gears
        |> Seq.length |> should equal 2
        
        schematic.gears
        |> Seq.map (fun g -> g.gearRatio) |> List.ofSeq
        |> should equal [16345; 451490]
        
        schematic.sumOfGearRatios
        |> should equal 467835
          
    [<Fact>]
    let ``2023 - Day 03 - part 2`` () =
        let schematic = day03input |> parser.parseInput
        
        schematic.sumOfGearRatios
        |> should equal 84289137
        // |> printfn "2023 - Day 03 - Part 2: %A"
