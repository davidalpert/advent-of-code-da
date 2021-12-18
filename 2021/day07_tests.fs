namespace AdventOfCode.Tests

module Day07 =

  open AdventOfCode.Input
  open AdventOfCode.Crabs
  open Xunit
  open FsUnit.Xunit

  let costToMoveTo (startingPositions:int[]) (x:int) =
    startingPositions
    |> Array.map (fun i -> abs (i - x) )
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

    costToMoveTo m n
    |> should equal cost

  let minCostBySeq (m:int[]) =
    let max = m |> Array.max
    let min = m |> Array.min

    seq { min .. max }
    |> Seq.map (costToMoveTo m)
    |> Seq.min

  [<Fact>]
  let ``Day 07 - part 1 - example`` () =
    let m =
      day07sample.Trim().Split(",")
      |> Array.map int

    minCostBySeq m
    |> should equal 37

  [<Fact>]
  let ``Day 07 - part 1 - calculation`` () =
    let m =
      day07data.Trim().Split(",")
      |> Array.map int

    minCostBySeq m
    |> should equal 355764