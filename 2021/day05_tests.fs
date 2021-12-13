namespace AdventOfCode.Tests

module Day05 =

  open AdventOfCode.Input
  open AdventOfCode.Helm
  open AdventOfCode.Hydrothermals
  open Xunit
  open FsUnit.Xunit

  [<Fact>]
  let ``Day 05 - Part 1 - LineSegement covers Points vertical`` () =
    let s1 = {
      p1 = { x = 1; y = 1; };
      p2 = { x = 1; y = 3; };
    }

    [
      { x = 1; y = 1; };
      { x = 1; y = 2; };
      { x = 1; y = 3; };
    ]
    |> List.iter (fun p -> 
      s1.covers p |> should equal true
    )

    s1.coveredHorizontallyOrVertically
    |> List.ofSeq
    |> should equal [
      { x = 1; y = 1 };
      { x = 1; y = 2 };
      { x = 1; y = 3 }
    ]

  [<Fact>]
  let ``Day 05 - Part 1 - LineSegement covers Points horizontal`` () =
    let s2 = {
      p1 = { x = 9; y = 7; };
      p2 = { x = 7; y = 7; };
    }

    [
      { x = 9; y = 7; };
      { x = 8; y = 7; };
      { x = 7; y = 7; };
    ]
    |> List.iter (fun p -> 
      s2.covers p |> should equal true
    )

    s2.coveredHorizontallyOrVertically
    |> List.ofSeq
    |> should equal [
      { x = 9; y = 7 };
      { x = 8; y = 7 };
      { x = 7; y = 7 };
    ]

  [<Fact>]
  let ``Day 05 - Part 1 - Parsing`` () =
    day05sample
    |> ventsFromInput
    |> should equal [
     { p1 = { x = 0; y = 9 }; p2 = { x = 5; y = 9 } };
     { p1 = { x = 8; y = 0 }; p2 = { x = 0; y = 8 } };
     { p1 = { x = 9; y = 4 }; p2 = { x = 3; y = 4 } };
     { p1 = { x = 2; y = 2 }; p2 = { x = 2; y = 1 } };
     { p1 = { x = 7; y = 0 }; p2 = { x = 7; y = 4 } };
     { p1 = { x = 6; y = 4 }; p2 = { x = 2; y = 0 } };
     { p1 = { x = 0; y = 9 }; p2 = { x = 2; y = 9 } };
     { p1 = { x = 3; y = 4 }; p2 = { x = 1; y = 4 } };
     { p1 = { x = 0; y = 0 }; p2 = { x = 8; y = 8 } };
     { p1 = { x = 5; y = 5 }; p2 = { x = 8; y = 2 } };
    ]

  // [<Fact>]
  // let ``Day 05 - Part 1 - Example`` () =
  //   day05sample
  //   |> ventsFromInput
  //   |> should equal 150

  [<Fact>]
  let ``Day 05 - Part 1 - Calculation`` () =
    day02data
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarine initialPosition
    |> (fun p -> p.displacement)
    |> should equal 2_073_315