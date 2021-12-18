namespace AdventOfCode.Tests

module Day07 =

  open AdventOfCode.Input
  open Xunit
  open FsUnit.Xunit

  // Each change of 1 step in horizontal position of a single crab costs 1 fuel. 
  let part1FuelCost (finalPosition:int) (initialPosition:int) =
    abs (initialPosition - finalPosition)

  let costToMoveTo (startingPositions:int[]) (fuelCoster:int -> int -> int) (x:int) =
    startingPositions
    |> Array.map (fuelCoster x)
    |> Array.sum

  [<Theory>]
  [<InlineData(0,49)>]
  [<InlineData(1,41)>]
  [<InlineData(2,37)>]
  [<InlineData(3,39)>]
  [<InlineData(4,41)>]
  [<InlineData(5,45)>]
  [<InlineData(6,49)>]
  [<InlineData(7,53)>]
  [<InlineData(8,59)>]
  [<InlineData(9,65)>]
  [<InlineData(10,71)>]
  [<InlineData(11,77)>]
  [<InlineData(12,83)>]
  [<InlineData(13,89)>]
  [<InlineData(14,95)>]
  [<InlineData(15,103)>]
  [<InlineData(16,111)>]
  let ``Day 07 - simple costToMoveTo`` (n:int, cost:int) =
    let m =
      day07sample.Trim().Split(",")
      |> Array.map int

    costToMoveTo m part1FuelCost n
    |> should equal cost

  let minCostBySeq (m:int[]) (fuelCoster:int -> int -> int) =
    let max = m |> Array.max
    let min = m |> Array.min

    seq { min .. max }
    |> Seq.map (costToMoveTo m fuelCoster)
    |> Seq.min

  [<Fact>]
  let ``Day 07 - part 1 - example`` () =
    let m =
      day07sample.Trim().Split(",")
      |> Array.map int

    minCostBySeq m part1FuelCost
    |> should equal 37

  [<Fact>]
  let ``Day 07 - part 1 - calculation`` () =
    let m =
      day07data.Trim().Split(",")
      |> Array.map int

    minCostBySeq m part1FuelCost
    |> should equal 355764

  // As it turns out, crab submarine engines don't burn fuel at a constant rate. Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last: the first step costs 1, the second step costs 2, the third step costs 3, and so on.
  let part2FuelCost (finalPosition:int) (initialPosition:int) =
    let s = sign (finalPosition - initialPosition)
    match s with
    | 0 -> 0
    | _ -> seq { initialPosition .. s .. finalPosition }
           |> Seq.map (fun p -> abs (p - initialPosition))
           |> Seq.sum
    
  [<Theory>]
  [<InlineData(16,5,66)>]
  [<InlineData(1,5,10)>]
  [<InlineData(2,5,6)>]
  [<InlineData(0,5,15)>]
  [<InlineData(4,5,1)>]
  [<InlineData(7,5,3)>]
  [<InlineData(14,5,45)>]
  let ``Day 07 - compound costToMoveTo`` (x1:int, x2:int, expectedCost:int) =
    part2FuelCost x1 x2
    |> should equal expectedCost
    
  [<Fact>]
  let ``Day 07 - part 2 - example`` () =
    let m =
      day07sample.Trim().Split(",")
      |> Array.map int

    minCostBySeq m part2FuelCost
    |> should equal 168

  [<Fact(Skip="not an efficient implementation")>]
  let ``Day 07 - part 2 - calculation`` () =
    let m =
      day07data.Trim().Split(",")
      |> Array.map int

    minCostBySeq m part2FuelCost
    |> should equal 99634572
