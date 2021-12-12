namespace AdventOfCode.Tests

module Day03 =

  open AdventOfCode.Input
  open AdventOfCode.Diagnostics
  open Xunit
  open FsUnit.Xunit

  [<Fact>]
  let ``Day 03 - Part 1 - Example`` () =
    day03sample
    |> diagnosticReportFromInput
    |> powerConsumption
    |> should equal (198 |> int64)

  [<Fact>]
  let ``Day 03 - Part 1 - Calculation`` () =
    day03data
    |> diagnosticReportFromInput
    |> powerConsumption
    |> should equal (1458194 |> int64)

  [<Fact>]
  let ``Day 03 - Part 2 - Example`` () =
    day03sample
    |> diagnosticReportFromInput
    |> lifeSupportRating
    |> should equal (230 |> int64)

  [<Fact>]
  let ``Day 03 - Part 2 - Calculation`` () =
    day03data
    |> diagnosticReportFromInput
    |> lifeSupportRating
    |> should equal (2829354 |> int64)
