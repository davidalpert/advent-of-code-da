namespace AdventOfCode

module Day05 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.day05_Print_Queue
    open AdventOfCode.Day05Input
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"""

    [<Fact>]
    let ``2024 - Day 05 - part 1 - example`` () =
        exampleInput
        |> parser.parseInput
        |> solve |> fst
        |> should equal 143

    [<Fact>]
    let ``2024 - Day 05 - part 1`` () =
        day05input
        |> parser.parseInput
        |> solve |> fst
        |> should equal 7024
        // |> printfn "2024 - Day 05 - Part 1: %A"

    // [<Fact>]
    let ``2024 - Day 05 - part 2 - example`` () =
        exampleInput
        |> parser.parseInput
        |> solve |> snd
        |> should equal 0

    // [<Fact>]
    let ``2024 - Day 05 - part 2`` () =
        day05input
        // |> fromInput
        // |> Array.length
        |> printfn "2024 - Day 05 - Part 2: %A"
