namespace AdventOfCode.Tests

module Day01 =

  open AdventOfCode.Input
  open AdventOfCode.NotQuiteLisp
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData("(())", 0)>]
  [<InlineData("()()", 0)>]
  [<InlineData("(((", 3)>]
  [<InlineData("(()(()(", 3)>]
  [<InlineData("))(((((", 3)>]
  [<InlineData("())", -1)>]
  [<InlineData("))(", -1)>]
  [<InlineData(")))", -3)>]
  [<InlineData(")())())", -3)>]
  let ``Day 1 - tests - follow directions`` (input:string, expectedFloor:int) =
    input
    |> followDirections
    |> should equal expectedFloor

  [<Fact>]
  let ``Day 01 - Part 1 - calculation`` () =
    day01data
    |> followDirections
    |> should equal 280 
