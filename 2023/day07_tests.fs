namespace AdventOfCode

open AdventOfCode.day07_Camel_Cards

module Day07 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.day07_Camel_Cards
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

    [<Fact>]
    let ``2023 - Day 07 - part 1 - example`` () =
        let set = exampleInput |> parser.parseInput parser.Part1
        
        set |> Array.skip 0 |> Array.head |> should equal {
              cards = [|CardValue.Three; CardValue.Two; CardValue.Ten; CardValue.Three; CardValue.King|];
              Type = HandType.OnePair;
              bid = 765
            }
        set |> Array.skip 1 |> Array.head |> should equal {
              cards = [|CardValue.Ten; CardValue.Five; CardValue.Five; CardValue.Jack; CardValue.Five|];
              Type = HandType.ThreeOfAKind;
              bid = 684
            }
        set |> Array.skip 2 |> Array.head |> should equal {
               cards = [|CardValue.King; CardValue.King; CardValue.Six; CardValue.Seven; CardValue.Seven|]
               Type = HandType.TwoPair
               bid = 28
            }
        set |> Array.skip 3 |> Array.head |> should equal {
               cards = [|CardValue.King; CardValue.Ten; CardValue.Jack; CardValue.Jack; CardValue.Ten|]
               Type = HandType.TwoPair
               bid = 220
            }
        set |> Array.skip 4 |> Array.head |> should equal {
               cards = [|CardValue.Queen; CardValue.Queen; CardValue.Queen; CardValue.Jack; CardValue.Ace|]
               Type = HandType.ThreeOfAKind
               bid = 483
            }
        
        // So, 33332 and 2AAAA are both four of a kind hands,
        // but 33332 is stronger because its first card is stronger.
        let ex1a = Hand.build "33332" 0
        let ex1b = Hand.build "2AAAA" 0
        ex1a.isStrongerThan ex1b |> should equal true
        
        // Similarly, 77888 and 77788 are both a full house,
        // but 77888 is stronger because its third card is stronger
        // (and both hands have the same first and second card).
        let ex2a = Hand.build "77888" 0
        let ex2b = Hand.build "77788" 0
        ex2a.isStrongerThan ex2b |> should equal true
        
        // rank reversed to read top-to-bottom as high-to-low
        rank set |> Array.map (fun c -> c.str) |> Array.rev |> should equal [|
            (* 5 *) "QQQJA ThreeOfAKind 483"
            (* 4 *) "T55J5 ThreeOfAKind 684"
            (* 3 *) "KK677 TwoPair 28"
            (* 2 *) "KTJJT TwoPair 220"
            (* 1 *) "32T3K OnePair 765"
        |]
        
        // (765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5)
        set |> totalWinnings |> should equal 6440

    // [<Fact>]
    let ``2023 - Day 07 - part 1`` () =
        day07input
        |> parser.parseInput parser.Part1
        |> totalWinnings
        // |> printfn "2023 - Day 07 - Part 1: %A"
        |> should equal 251216224

    [<Fact>]
    let ``2023 - Day 07 - part 2 - example`` () =
        let ex1a = (Hand.part2build "QJJQ2" 0)
        let ex1b = (Hand.part2build "QQQQ2" 0)

        ex1a.Type |> should equal HandType.FourOfAKind
        ex1a.isStrongerThan ex1b |> should equal false

        (Hand.part2build "32T3K" 0).Type |> should equal HandType.OnePair
        (Hand.part2build "KK677" 0).Type |> should equal HandType.TwoPair
        (Hand.part2build "T55J5" 0).Type |> should equal HandType.FourOfAKind
        (Hand.part2build "KTJJT" 0).Type |> should equal HandType.FourOfAKind
        (Hand.part2build "QQQJA" 0).Type |> should equal HandType.FourOfAKind

        let set = exampleInput |> parser.parseInput parser.Part2

        // rank reversed to read top-to-bottom as high-to-low
        rank set |> Array.map (fun c -> c.str) |> Array.rev |> should equal [|
            (* 5 *) "KTJJT FourOfAKind 220"
            (* 4 *) "QQQJA FourOfAKind 483"
            (* 3 *) "T55J5 FourOfAKind 684"
            (* 2 *) "KK677 TwoPair 28"
            (* 1 *) "32T3K OnePair 765"
        |]

    // [<Fact>]
    let ``2023 - Day 07 - part 2`` () =
        day07input
        |> parser.parseInput parser.Part2
        |> totalWinnings
        // |> printfn "2023 - Day 07 - Part 2: %A"
        |> should equal 250825971
