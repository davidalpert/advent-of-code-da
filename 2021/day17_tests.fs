namespace AdventOfCode

module Day17 =

  open Input
  open Xunit
  open FsUnit.Xunit
  open ProbeLauncher
  open TargetAreaParser

  // let day17input = "target area: x=156..202, y=-110..-69"

  [<Fact>]
  let ``Day 17 - tests - parse sample target area`` () =
    let input = "target area: x=20..30, y=-10..-5"

    let r = parse input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result |> should equal {
        p1 = { x = 20; y = -10 };
        p2 = { x = 30; y =  -5 };
      }

  [<Theory>]
  [<InlineData("target area: x=20..30, y=-10..-5",  0,  0, false)>]
  [<InlineData("target area: x=20..30, y=-10..-5", 20, -5, true)>]
  let ``Day 17 - tests - area contains position`` (input, x, y, expectedInside) =
    let r = parse input
    let p = { x = x; y = y; }

    match r with
    | Error(e)   -> failwith e
    | Ok(area) ->
      (area.contains p) |> should equal expectedInside

  [<Theory>]
  [<InlineData("target area: x=20..30, y=-10..-5", 17,  -4, false)>]
  [<InlineData("target area: x=20..30, y=-10..-5", 33,  -9, false)>]
  [<InlineData("target area: x=20..30, y=-10..-5", 48, -15, true)>]
  let ``Day 17 - tests - position is past area`` (input, x, y, expectedPast) =
    let r = parse input
    let p = { x = x; y = y; }

    match r with
    | Error(e)   -> failwith e
    | Ok(area) ->
      (p.isPast area) |> should equal expectedPast