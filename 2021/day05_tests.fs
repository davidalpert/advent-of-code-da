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

  [<Fact>]
  let ``Day 05 - Part 1 - Example`` () =
    let scan =
      day05sample
      |> ventsFromInput
      |> List.filter segmentIsHorizontalOrVertical
      |> ThermalScan

    // printfn "example: found %d vented points" scan.ventedPoints.Keys.Count
    
    scan.ventedPoints
    |> Seq.filter (fun pair -> pair.Value >= 2)
    |> Seq.length
    |> should equal 5

  [<Fact>]
  let ``Day 05 - Part 1 - Calculation`` () =
    let scan =
      day05data
      |> ventsFromInput
      |> List.filter segmentIsHorizontalOrVertical
      |> ThermalScan

    // printfn "data: found %d vented points" scan.ventedPoints.Keys.Count
    
    scan.ventedPoints
    |> Seq.filter (fun pair -> pair.Value >= 2)
    |> Seq.length
    |> should equal 5442

  [<Fact>]
  let ``Day 05 - Part 2 - LineSegement covers Points diagonal`` () =
    let s1 = {
      p1 = { x = 1; y = 1; };
      p2 = { x = 3; y = 3; };
    }

    s1.coveredDiagonally
    |> List.ofSeq
    |> should equal [
      { x = 1; y = 1 };
      { x = 2; y = 2 };
      { x = 3; y = 3 }
    ]

  [<Fact>]
  let ``Day 05 - Part 1 - LineSegement covers Points diagonally 2`` () =
    let s2 = {
      p1 = { x = 9; y = 7; };
      p2 = { x = 7; y = 9; };
    }

    s2.coveredDiagonally
    |> List.ofSeq
    |> should equal [
      { x = 9; y = 7 };
      { x = 8; y = 8 };
      { x = 7; y = 9 };
    ]

  [<Fact>]
  let ``Day 05 - Part 2 - Example`` () =
    let scan =
      day05sample
      |> ventsFromInput
      |> ThermalScan

    printfn "%s" (scan.ventedPoints2 |> diagram)
    // printfn "example: found %d vented points" scan.ventedPoints.Keys.Count

    scan.ventedPoints2
    |> Seq.filter (fun pair -> pair.Value >= 2)
    |> Seq.length
    |> should equal 12

  [<Fact>]
  let ``Day 05 - Part 2 - Data`` () =
    let scan =
      day05data
      |> ventsFromInput
      |> ThermalScan

    // printfn "%s" (scan.ventedPoints2 |> diagram)
    // printfn "example: found %d vented points" scan.ventedPoints.Keys.Count

    scan.ventedPoints2
    |> Seq.filter (fun pair -> pair.Value >= 2)
    |> Seq.length
    |> should equal 19571
