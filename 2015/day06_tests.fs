namespace AdventOfCode.Tests

module Day06 =

  open AdventOfCode.Input
  open AdventOfCode.FireHazard
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData("turn on 0,0 through 999,999", 1_000_000)>] 
  [<InlineData("turn on 499,499 through 500,500", 4)>] 
  [<InlineData("toggle 0,0 through 999,0", 1000)>] 
  [<InlineData("turn on 0,0 through 999,0", 1000)>] 
  let ``Day 6 - part 1 - tests`` (input:string, expected:int) =
    let i =
      input
      |> Instruction.fromString

    [|i|]
    |> applyInstructions
    |> Seq.length
    |> should equal expected

  // [<Fact>]
  let ``Day 6 - part 1 - calculation``() =
    day06input
    |> toInstructions
    |> applyInstructions
    |> Seq.length
    |> should equal 400410

  [<Theory>]
  [<InlineData("turn on 0,0 through 0,0", 1)>] 
  [<InlineData("toggle 0,0 through 999,999", 2000000)>] 
  let ``Day 6 - part 2 - tests`` (input:string, expected:int) =
    let i =
      input
      |> Instruction.fromString

    [|i|]
    |> applyInstructions2
    |> Seq.sum
    |> should equal expected

  [<Fact>]
  let ``Day 6 - part 2 - calculation``() =
    day06input
    |> toInstructions
    |> applyInstructions2
    |> Seq.sum
    |> should equal 15343601
