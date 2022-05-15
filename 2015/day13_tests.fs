namespace AdventOfCode.Tests

module Day13 =

    open AdventOfCode.Input
    open AdventOfCode.SeatingArrangements
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.
"""

    [<Theory>]
    [<InlineData("Alice", "Bob", 54)>]
    [<InlineData("Bob", "Alice", 83)>]
    [<InlineData("Alice", "David", -2)>]
    let ``Day13 - part 1 - happinessMap`` (a, b, expected) =
        exampleInput
        |> toImpactMap
        |> (fun m -> m.[(a, b)])
        |> should equal expected

    [<Fact>]
    let ``Day13 - part 1 - examples`` () =
        exampleInput
        |> findHappiestAggangement false
        |> snd
        |> should equal 330

    [<Fact>]
    let ``Day13 - part 1 - calculation`` () =
        day13input
        |> findHappiestAggangement false
        |> snd
        |> should equal 664
