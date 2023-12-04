namespace AdventOfCode

module Day04 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.day04_Scratchcards
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

(*
In the above example, card 1 has five winning numbers (41, 48, 83, 86, and 17) and
eight numbers you have (83, 86, 6, 31, 17, 9, 48, and 53). Of the numbers you have,
four of them (48, 83, 17, and 86) are winning numbers! That means card 1 is worth 8
points (1 for the first match, then doubled three times for each of the three
matches after the first).

Card 2 has two winning numbers (32 and 61), so it is worth 2 points.
Card 3 has two winning numbers (1 and 21), so it is worth 2 points.
Card 4 has one winning number (84), so it is worth 1 point.
Card 5 has no winning numbers, so it is worth no points.
Card 6 has no winning numbers, so it is worth no points.
So, in this example, the Elf's pile of scratchcards is worth 13 points.
*)
    [<Fact>]
    let ``2023 - Day 04 - scratchcards`` () =
        let c = Card.build(1, [|41;48;83;86;17|], [|83;86;6;31;17;9;48;53|])
        
        c.matches
        |> Array.sort
        |> should equal ([|48; 83; 17; 86|] |> Array.sort)

        c.points
        |> should equal 8

    [<Theory>]
    [<InlineData(1, 8)>]
    [<InlineData(2, 2)>]
    [<InlineData(3, 2)>]
    [<InlineData(4, 1)>]
    [<InlineData(5, 0)>]
    [<InlineData(6, 0)>]
    let ``2023 - Day 04 - part 1 - points per card`` (id, expectedPointValue) =
        let c =
            exampleInput
            |> parser.parseInput
            |> Seq.skip (id - 1) |> Seq.head
            
        c.points
        |> should equal expectedPointValue
        
    [<Fact>]
    let ``2023 - Day 04 - part 1 - example`` () =
        exampleInput
        |> parser.parseInput
        |> sumOfPoints
        |> should equal 13

    [<Fact>]
    let ``2023 - Day 04 - part 1`` () =
        day04input
        |> parser.parseInput
        |> sumOfPoints
        // |> printfn "2023 - Day 04 - Part 1: %A"
        |> should equal 21485
        
    [<Theory>]
    [<InlineData(1,  1)>]
    [<InlineData(2,  2)>]
    [<InlineData(3,  4)>]
    [<InlineData(4,  8)>]
    [<InlineData(5, 14)>]
    [<InlineData(6,  1)>]
    let ``2023 - Day 04 - part 2 - final copies per card`` (id, expectedNumberOfCopies) =
        let c =
            exampleInput
            |> parser.parseInput
            |> countsByCardPart2
            
        c[id]
        |> should equal expectedNumberOfCopies
        
    [<Fact>]
    let ``2023 - Day 04 - part 2 - example`` () =
        exampleInput
        |> parser.parseInput
        |> countOfTotalCards2
        // |> sprintf "%A"
        |> should equal 30

    [<Fact>]
    let ``2023 - Day 04 - part 2`` () =
        day04input
        |> parser.parseInput
        |> countOfTotalCards2
        // |> printfn "2023 - Day 04 - Part 2: %A"
        |> should equal 11024379
