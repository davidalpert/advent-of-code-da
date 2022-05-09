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

  [<Theory>]
  [<InlineData(")", 1)>]
  [<InlineData("()())", 5)>]
  let ``Day 1 - part 2 - tests`` (input:string, expectedPosition:int) =
    input
    |> positionWhichEntersBasement
    |> should equal expectedPosition

  [<Fact>]
  let ``Day 01 - Part 2 - calculation`` () =
    day01data
    |> positionWhichEntersBasement
    |> should equal 1797 
