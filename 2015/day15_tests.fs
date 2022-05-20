module Day15 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.ScienceForHungryPeople
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
"""

    let puzzleInput =
        """
Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1
"""

    [<Fact>]
    let ``validate fromInput`` () =
        exampleInput
        |> fromInput
        |> Array.length
        |> should equal 2

    puzzleInput
    |> fromInput
    |> Array.length
    |> should equal 4

    [<Theory>]
    [<InlineData(1, 5, 1, false)>]
    [<InlineData(2, 5, 6, false)>]
    [<InlineData(3, 5, 21, false)>]
    let ``first we figure out the cartesian product between n possibilities between 1 and k`` (n, k, m, debug) =
        let prn x =
            if debug then
                printfn "%d of %d makes: %A" n k x

            x

        recipeCombinationsOfN n k
        |> prn
        |> Seq.length
        |> should equal m

    [<Fact>]
    let ``Day 15 - Part 1 - sample`` () =
        exampleInput
        |> findHighestScoringCookieRecipe 100
        |> snd
        |> should equal 62842880

    [<Fact>]
    let ``Day 15 - Part 1 - calculation`` () =
        puzzleInput
        |> findHighestScoringCookieRecipe 100
        |> snd
        |> should equal 18965440
