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

  [<Theory>]
  [<InlineData("2x3x4", 10, 24)>]
  [<InlineData("1x1x10", 4, 10)>]
  let ``Day 2 - Part 2 - tests - square footage for a single gift`` (input:string, expectedToWrap:int, expectedForBow:int) =
    input
    |> singleGiftToDimensions
    |> lengthToWrap
    |> should equal expectedToWrap

    input
    |> singleGiftToDimensions
    |> lengthForBow
    |> should equal expectedForBow

    input 
    |> singleGiftToDimensions
    |> lengthOfRibbonRequired
    |> should equal (expectedToWrap + expectedForBow)

  [<Fact>]
  let ``Day 2 - part 2 - calculation``() =
    day02data
    |> inputToGiftDimensions
    |> Seq.map lengthOfRibbonRequired
    |> Seq.sum
    |> should equal 3737498
