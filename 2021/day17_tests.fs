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
  [<InlineData("target area: x=20..30, y=-10..-5", 28, -7, true)>]
  let ``Day 17 - tests - area contains position`` (input, x, y, expectedContains) =
    let r = parse input
    let p = { x = x; y = y; }

    match r with
    | Error(e)   -> failwith e
    | Ok(area) ->
      (area.contains p) |> should equal expectedContains

  [<Theory>]
  [<InlineData("target area: x=20..30, y=-10..-5", 17,  -4, false)>]
  [<InlineData("target area: x=20..30, y=-10..-5", 28,  -7, false)>]
  [<InlineData("target area: x=20..30, y=-10..-5", 33,  -9, true)>]
  [<InlineData("target area: x=20..30, y=-10..-5", 48, -15, true)>]
  let ``Day 17 - tests - position is past area`` (input, x, y, expectedPast) =
    let r = parse input
    let p = { x = x; y = y; }

    match r with
    | Error(e)   -> failwith e
    | Ok(area) ->
      (p.isPast area) |> should equal expectedPast

  [<Theory>]
  [<InlineData("target area: x=20..30, y=-10..-5", 0,  10, false)>]
  [<InlineData("target area: x=20..30, y=-10..-5", 7,  2, true)>]
  [<InlineData("target area: x=20..30, y=-10..-5", 6, 3, true)>]
  [<InlineData("target area: x=20..30, y=-10..-5", 6, 9, true)>]
  let ``Day 17 - tests - ends inside`` (input, x, y, expectedInside) =
    let r = parse input
    let v = { vx = x; vy = y; }

    match r with
    | Error(e)   -> failwith e
    | Ok(area) ->
      (v |> area.mapTrajectory |> area.endsInside) |> should equal expectedInside

  [<Fact>]
  let ``Day 17 - tests - map a trajectory`` () =
    let input = "target area: x=20..30, y=-10..-5"
    let vInitial = { vx = 7; vy = 2 }
    let expectedTrajectory = [
      { x =  0; y =  0 };
      { x =  7; y =  2 };
      { x = 13; y =  3 };
      { x = 18; y =  3 };
      { x = 22; y =  2 };
      { x = 25; y =  0 };
      { x = 27; y = -3 };
      { x = 28; y = -7 }
    ]

    let r = parse input

    match r with
    | Error(e)   -> failwith e
    | Ok(area) ->
      (area.mapTrajectory vInitial)
      |> List.map fst
      |> should equal expectedTrajectory

// 0         1         2         3
// 0123456789012345678901234567890
// .............#....#............   3
// .......#..............#........   2
// ...............................   1
// S........................#.....   0
// ...............................  -1
// ...............................  -2
// ...........................#...  -3
// ...............................  -4
// ....................TTTTTTTTTTT  -5
// ....................TTTTTTTTTTT  -6
// ....................TTTTTTTT#TT  -7
// ....................TTTTTTTTTTT  -8
// ....................TTTTTTTTTTT  -9
// ....................TTTTTTTTTTT -10

  [<Fact>]
  let ``Day 17 - tests - render trajectory 7,2`` () =

    let input = "target area: x=20..30, y=-10..-5"
    let vInitial = { vx = 7; vy = 2 }
    let expectedRender = """
.............#....#............
.......#..............#........
...............................
S........................#.....
...............................
...............................
...........................#...
...............................
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTT#TT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
"""

    let r = parse input

    match r with
    | Error(e)   -> failwith e
    | Ok(area) ->
      (area.render vInitial) |> should equal expectedRender

  [<Fact>]
  let ``Day 17 - tests - render trajectory 6,3`` () =

    let input = "target area: x=20..30, y=-10..-5"
    let vInitial = { vx = 6; vy = 3 }
    let expectedRender = """
...............#..#............
...........#........#..........
...............................
......#..............#.........
...............................
...............................
S....................#.........
...............................
...............................
...............................
.....................#.........
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................T#TTTTTTTTT
....................TTTTTTTTTTT
"""

    let r = parse input

    match r with
    | Error(e)   -> failwith e
    | Ok(area) ->
      (area.render vInitial) |> should equal expectedRender

  [<Fact(Skip="silenced")>]
  let ``Day 17 - tests - allPossibleVelocitiesLessThan`` () =
    let input = "target area: x=20..30, y=-10..-5"
    let r = parse input

    match r with
    | Error(e) -> failwith e
    | Ok(area) ->

      area.allPossibleVelocitiesLessThan 9
      |> Seq.map area.mapTrajectory
      |> Seq.filter area.endsInside
      |> Seq.maxBy highestPoint
      |> greatestHeight
      |> should equal 45 

  [<Fact>]
  let ``Day 17 - part 1 - calculation`` () =
    let r = parse day17input

    match r with
    | Error(e) -> failwith e
    | Ok(area) ->

      area.allPossibleVelocitiesLessThan 300
      |> Seq.map area.mapTrajectory
      |> Seq.filter area.endsInside
      // |> Seq.filter (fun t -> (t |> greatestHeight) > 0 )
      |> Seq.maxBy highestPoint
      |> greatestHeight
      |> should equal 5995 
