namespace AdventOfCode.Tests

module Day01 =

  open AdventOfCode.Input
  open AdventOfCode.ReportRepair
  open Xunit
  open FsUnit.Xunit

  [<Fact>]
  let ``Day 01 - Part 1 - find entries - sample`` () =
    day01sample
    |> integersFromInput
    |> entriesWhichSumTo 2020
    |> should equal (1721, 299) 

  [<Fact>]
  let ``Day 01 - Part 1 - find entries - input`` () =
    day01data
    |> integersFromInput
    |> entriesWhichSumTo 2020
    |> should equal (1093, 927) 

  [<Fact>]
  let ``Day 01 - Part 1 - Example`` () =
    day01sample
    |> integersFromInput
    |> multipleOfEntriesWhichSumTo 2020
    |> should equal 514579

  [<Fact>]
  let ``Day 01 - Part 1 - Calculation`` () =
    day01data
    |> integersFromInput
    |> multipleOfEntriesWhichSumTo 2020
    |> should equal 1013211

  // [<Fact>]
  // let ``Day 01 - Part 2 - Example`` () =
  //   day01sample
  //   |> soundingsFromInput
  //   |> numberOfTimesDepthIncreasesByWindow 3
  //   |> should equal 5

  // [<Fact>]
  // let ``Day 01 - Part 2 - Calculation`` () =
  //   day01data
  //   |> soundingsFromInput
  //   |> numberOfTimesDepthIncreasesByWindow 3
  //   |> should equal 1150
