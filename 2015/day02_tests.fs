namespace AdventOfCode.Tests

module Day02 =

  open AdventOfCode.Input
  open AdventOfCode.WrappingPaper
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData("2x3x4", 58)>]
  [<InlineData("1x1x10", 43)>]
  let ``Day 2 - tests - square footage for a single gift`` (input:string, expectedSquareFeet:int) =
    input
    |> singleGiftToDimensions
    |> squareFootagePlusSlack
    |> should equal expectedSquareFeet

  [<Fact>]
  let ``Day 2 - calculation - square footage for all the gifts``() =
    day02data
    |> inputToGiftDimensions
    |> Seq.map squareFootagePlusSlack
    |> Seq.sum
    |> should equal 1586300