namespace AdventOfCode

module Day13 =

  open Input
  open Xunit
  open FsUnit.Xunit
  open Origami
  open Origami.AST

  [<Fact>]
  let ``Day 13 - tests - parse simple input`` () =
    let input = """
3,0
8,4

fold along y=3
"""
    let r = Parser.parse input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> 
      result.foldInstructions.Length |> should equal 1

      "\n" + result.render + "\n" |> should equal """
...#.....
.........
.........
.........
........#
"""

      "\n" + result.fold.render + "\n" |> should equal """
...#.....
.........
........#
"""

  [<Fact>]
  let ``Day 13 - tests - parse sample input`` () =
    let r = Parser.parse day13sample

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> 
      // printfn "\n%s\n" result.render
      1 |> should equal 1

  [<Fact>]
  let ``Day 13 - tests - parse actual input`` () =
    let r = Parser.parse day13data

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> 
      1 |> should equal 1

  [<Fact>]
  let ``Day 13 - tests - sample input first fold`` () =
    let r = Parser.parse day13sample

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> 

      result.foldInstructions.Length |> should equal 2

      "\n" + (result.foldNTimes 1).render + "\n" |> should equal """
#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
"""

  [<Fact>]
  let ``Day 13 - tests - sample input fold`` () =
    let r = Parser.parse day13sample

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> 

      result.foldInstructions.Length |> should equal 2

      "\n" + result.fold.render + "\n" |> should equal """
#####
#...#
#...#
#...#
#####
"""