namespace AdventOfCode.Tests

module Day01 =

  open AdventOfCode.Input
  open AdventOfCode.DepthSounder
  open Xunit
  open FsUnit.Xunit

  [<Fact>]
  let ``Day 01 - Part 1 - Example`` () =
    day01sample
    |> soundingsFromInput
    |> numberOfTimesDepthIncreases
    |> should equal 7

  [<Fact>]
  let ``Day 01 - Part 1 - Calculation`` () =
    day01data
    |> soundingsFromInput
    |> numberOfTimesDepthIncreases
    |> should equal 1215

  [<Fact>]
  let ``Day 01 - Part 2 - Example`` () =
    day01sample
    |> soundingsFromInput
    |> numberOfTimesDepthIncreasesByWindow 3
    |> should equal 5

  [<Fact>]
  let ``Day 01 - Part 2 - Calculation`` () =
    day01data
    |> soundingsFromInput
    |> numberOfTimesDepthIncreasesByWindow 3
    |> should equal 1150
