namespace AdventOfCode.Tests

module Day02 =

  open AdventOfCode.Input
  open AdventOfCode.Helm
  open Xunit
  open FsUnit.Xunit

  [<Fact>]
  let ``Day 02 - Part 1 - Example`` () =
    day02sample
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarine initialPosition
    |> (fun p -> p.displacement)
    |> should equal 150

  [<Fact>]
  let ``Day 02 - Part 1 - Calculation`` () =
    day02data
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarine initialPosition
    |> (fun p -> p.displacement)
    |> should equal 2_073_315

  [<Fact>]
  let ``Day 02 - Part 2 - Example`` () =
    day02sample
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarineWithAim initialPosition
    |> (fun p -> p.displacement)
    |> should equal 900

  [<Fact>]
  let ``Day 02 - Part 2 - Calculation`` () =
    day02data
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarineWithAim initialPosition
    |> (fun p -> p.displacement)
    |> should equal 1_840_311_528
