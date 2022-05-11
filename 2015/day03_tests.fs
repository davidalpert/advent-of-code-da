namespace AdventOfCode.Tests

module Day03 =

  open AdventOfCode.Input
  open AdventOfCode.GiftDelivery
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData(">", 2)>]
  [<InlineData("^>v<", 4)>]
  [<InlineData("^v^v^v^v^v", 2)>]
  let ``Day 3 - tests - how many houses get a single gift`` (input:string, expectedN:int) =
    let visitedLocations =
      input
      |> deliveryInstructionsToGiftsPerLocation

    visitedLocations.Count
    |> should equal expectedN

  [<Fact>]
  let ``Day 3 - calculation - how many houses get a single gift`` () =
    let visitedLocations =
      day03input
      |> deliveryInstructionsToGiftsPerLocation

    visitedLocations.Count
    |> should equal 2081
