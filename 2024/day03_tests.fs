namespace AdventOfCode

module Day03 =
    open AdventOfCode.day03_Mull_It_Over
    open AdventOfCode.Day03Input
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
      // mul(2,4)                    mul(5,5)                mul(11,8)mul(8,5) "

    [<Fact>]
    let ``2024 - Day 03 - part 1 - example`` () =
        exampleInput
        |> parser.parseInput
        |> addUpResults
        |> should equal 161

    [<Fact>]
    let ``2024 - Day 03 - part 1`` () =
        day03input
        |> parser.parseInput
        |> addUpResults
        |> should equal 156388521
        // |> printfn "2024 - Day 03 - Part 1: %A"

    // [<Fact>]
    let ``2024 - Day 03 - part 2 - example`` () =
        exampleInput
        // |> fromInput
        // |> Array.length
        |> should equal 0

    // [<Fact>]
    let ``2024 - Day 03 - part 2`` () =
        day03input
        // |> fromInput
        // |> Array.length
        |> printfn "2024 - Day 03 - Part 2: %A"
