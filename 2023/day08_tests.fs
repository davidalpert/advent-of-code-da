namespace AdventOfCode

module Day08 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.day08_Haunted_Wasteland
    open Xunit
    open FsUnit.Xunit

    let example1input =
        """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""

    [<Fact>]
    let ``2023 - Day 08 - part 1 - example 1`` () =
        let nodeMap =
            example1input
            |> parser.parseInput

        nodeMap
        |> should equal {
            instruction_seed = [|R; L|]
            nodes_by_name = Map [
                ("AAA", { name = "AAA"; neighbors = ("BBB", "CCC") });
                ("BBB", { name = "BBB"; neighbors = ("DDD", "EEE") });
                ("CCC", { name = "CCC"; neighbors = ("ZZZ", "GGG") });
                ("DDD", { name = "DDD"; neighbors = ("DDD", "DDD") });
                ("EEE", { name = "EEE"; neighbors = ("EEE", "EEE") });
                ("GGG", { name = "GGG"; neighbors = ("GGG", "GGG") });
                ("ZZZ", { name = "ZZZ"; neighbors = ("ZZZ", "ZZZ") })
            ]
        }

        nodeMap
        |> follow
        |> Seq.length
        |> should equal 2

    let example2input =
        """
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

    [<Fact>]
    let ``2023 - Day 08 - part 1 - example 2`` () =
        let nodeMap =
            example2input
            |> parser.parseInput

        nodeMap
        |> should equal {
            instruction_seed = [|L; L; R|]
            nodes_by_name = Map [
                ("AAA", { name = "AAA"; neighbors = ("BBB", "BBB") });
                ("BBB", { name = "BBB"; neighbors = ("AAA", "ZZZ") });
                ("ZZZ", { name = "ZZZ"; neighbors = ("ZZZ", "ZZZ") })
            ]
        }

        nodeMap
        |> follow
        |> Seq.length
        |> should equal 6

    // [<Fact>]
    let ``2023 - Day 08 - part 1`` () =
        day08input
        |> parser.parseInput
        |> follow
        |> Seq.length
        // |> printfn "2023 - Day 08 - Part 1: %A"
        |> should equal 18023

    let example3input =
        """
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"""

    [<Fact>]
    let ``2023 - Day 08 - part 2 - example`` () =
        2 |> utils.primeFactorsOf |> Set |> should equal ([1;2] |> Set)
        [|2;3|] |> utils.lcmByVennDiagram |> should equal 6L
        
        let mp = example3input |> parser.parseInput
        mp
        |> part2FollowByMath
        |> should equal 6L

    [<Fact>]
    let ``2023 - Day 08 - part 2`` () =
        let mp = day08input |> parser.parseInput
        
        mp
        |> part2FollowByMath
        // |> printfn "2023 - Day 08 - Part 2: %A"
        // |> should equal 1175949435 // too low
        |> should equal 14449445933179L
        