namespace AdventOfCode

module day11 =

  open Input
  open Octopods
  open Xunit
  open FsUnit.Xunit

  [<Fact>]
  let ``Day 11 - test - Cavern.fromInput`` () =
    let c = Cavern.fromInput day11sampleCenter

    c.cells
    |> should equal [|
      [|1; 1; 1; 1; 1|];
      [|1; 9; 9; 9; 1|];
      [|1; 9; 1; 9; 1|];
      [|1; 9; 9; 9; 1|];
      [|1; 1; 1; 1; 1|]
    |]

    c.toString
    |> should equal """
11111
19991
19191
19991
11111
"""

  [<Fact>]
  let ``Day 11 - test - smaller steps`` () =
    let input = """
10
01
"""
    let c = Cavern.fromInput input

    (c.afterStep 1).toString
    |> should equal """
21
12
"""

  [<Fact>]
  let ``Day 11 - test - smaller steps 2`` () =
    let input = """
90
01
"""
    let c = Cavern.fromInput input

    (c.afterStep 1).toString
    |> should equal """
02
23
"""

//   [<Fact>]
//   let ``Day 11 - test - Cavern.step`` () =
//     let c = Cavern.fromInput day11sampleCenter

//     (c.afterStep 1).toString
//     |> should equal """
// 34543
// 40004
// 50005
// 40004
// 34543
// """