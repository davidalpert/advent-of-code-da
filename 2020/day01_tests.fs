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
    |> entriesWhichSumByNTo 2 2020
    |> should equal [1721; 299] 

  [<Fact>]
  let ``Day 01 - Part 1 - find entries - input`` () =
    day01data
    |> integersFromInput
    |> entriesWhichSumByNTo 2 2020
    |> should equal [1093; 927] 

  [<Fact>]
  let ``Day 01 - Part 1 - Example`` () =
    day01sample
    |> integersFromInput
    |> multipleOfEntriesWhichSumByNTo 2 2020
    |> should equal 514579

  [<Fact>]
  let ``Day 01 - Part 1 - Calculation`` () =
    day01data
    |> integersFromInput
    |> multipleOfEntriesWhichSumByNTo 2 2020
    |> should equal 1013211

  [<Fact>]
  let ``Day 01 - Part 2 - Example`` () =
    day01sample
    |> integersFromInput
    |> multipleOfEntriesWhichSumByNTo 3 2020
    |> should equal 241861950

  [<Fact>]
  let ``Day 01 - Part 2 - Calculation`` () =
    day01data
    |> integersFromInput
    |> multipleOfEntriesWhichSumByNTo 3 2020
    |> should equal 13891280
