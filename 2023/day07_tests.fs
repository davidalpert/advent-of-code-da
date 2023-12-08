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
        let set = exampleInput |> parser.parseInput
        
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
        |> parser.parseInput
        |> totalWinnings
        // |> printfn "2023 - Day 07 - Part 1: %A"
        |> should equal 251216224

    // [<Fact>]
    let ``2023 - Day 07 - part 2 - example`` () =
        exampleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2023 - Day 07 - part 2`` () =
        day07input
        // |> fromInput
        // |> Array.length
        |> printfn "2023 - Day 07 - Part 2: %A"
