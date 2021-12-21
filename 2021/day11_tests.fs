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
  let ``Day 11 - test - Cavern.step`` () =
    let c = Cavern.fromInput day11sampleCenter

    (c.afterStep 1).toString
    |> should equal """
34543
40004
50005
40004
34543
"""