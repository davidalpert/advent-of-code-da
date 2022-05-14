namespace AdventOfCode.Tests

module Day09 =

  open AdventOfCode.Input
  open AdventOfCode.AllInASingleDay
  open System.Collections.Generic
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData("""
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
""", 605)>]
  let ``Day 9 - part 1 - shortest path to all nodes`` (s:string, expected:int) =
    s
    |> splitToTrimmedLines
    |> calculatedShortestPathToVisitAllNodes
    |> should equal expected

  [<Fact>]
  let ``Day 9 - part 1 - calculation`` () =
    day09input
    |> splitToTrimmedLines
    |> calculatedShortestPathToVisitAllNodes
    |> should equal 251

  [<Fact>]
  let ``Day 9 - part 2 - calculation`` () =
    day09input
    |> splitToTrimmedLines
    |> calculatedLongestPathToVisitAllNodes
    |> should equal 898
