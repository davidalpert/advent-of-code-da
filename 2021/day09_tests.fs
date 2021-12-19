namespace AdventOfCode.Tests

module Day09 =

  open AdventOfCode.Input
  open Xunit
  open FsUnit.Xunit

  type pos = {
    x:int
    y:int
    h:int
  }

  let gridFromInput (input:string) =
    input.Trim().Split("\n")
    |> Array.map (fun s -> s.ToCharArray() |> Array.map int)

  let positionsFromInput (input:string) =
    // printfn "parsing %A" input
    input.Trim().Split("\n")
    |> Array.mapi (fun y s ->
      // printfn "- %A '%A'" y s
      s.ToCharArray()
      |> Array.map int
      |> Array.mapi (fun x h ->
        // printfn "  - %A '%A'" x (h - 48)
        { x = x; y = y; h = h - 48 }
      )
    )
    |> Array.concat

  let actualNeighbors (positions:pos[]) (p:pos) =
    positions
    |> Array.filter (fun pn -> 
      (* up *)    (pn.x = (p.x) && pn.y = (p.y - 1)) ||
      (* down *)  (pn.x = (p.x) && pn.y = (p.y + 1)) ||
      (* left *)  (pn.x = (p.x - 1) && pn.y = (p.y)) ||
      (* right *) (pn.x = (p.x + 1) && pn.y = (p.y))
    )

  let isLowerThanNeighbors (positions:pos[]) (p:pos) =
    actualNeighbors positions p
    |> Array.forall (fun n -> p.h > n.h)

  [<Fact>]
  let ``Day 09 - test - actualNeighbors`` () =
    let pos = positionsFromInput day09sample

    actualNeighbors pos { x = 0; y = 0; h = 0}
    |> should equal [|{ x = 1; y = 0; h = 1 }; { x = 0; y = 1; h = 3 }|]

    actualNeighbors pos { x = 1; y = 1; h = 0}
    |> should equal [|
      { x = 1; y = 0; h = 1 };
      { x = 0; y = 1; h = 3 };
      { x = 2; y = 1; h = 8 };
      { x = 1; y = 2; h = 8 };
    |]

  let lowPoints (positions:pos[]) =
    positions
    |> Array.map (fun p -> (p,(actualNeighbors positions p)))
    |> Array.filter (fun (p,nn) -> nn |> Array.forall (fun n -> p.h < n.h))
    |> Array.map (fun (p,_) -> p)

  let riskLevel (p:pos) =
    p.h + 1

  let totalRiskOfLowPoints (positions:pos[]) =
    lowPoints positions
    |> Array.map (fun p -> riskLevel p)
    |> Array.sum

  [<Fact>]
  let ``Day 09 - test - lowPoints`` () =
    let pos = positionsFromInput day09sample
    lowPoints pos
    |> Array.length
    |> should equal 4

  [<Fact>]
  let ``Day 09 - part 1 - example`` () =
    let pos = positionsFromInput day09sample

    totalRiskOfLowPoints pos
    |> should equal 15

  [<Fact>]
  let ``Day 08 - part 1 - calculation`` () =
    let pos = positionsFromInput day09data
    totalRiskOfLowPoints pos
    |> should equal 532

  // type basin(pp:pos seq) =

  //   member x.positions = pp |> List.ofSeq

  //   member x.contains (p:pos) =
  //     x.positions |> List.contains p 

  //   member x.addOne (p:pos) =
  //     basin(p :: x.positions)

  //   member x.addSome (ps:pos seq) =
  //     basin(List.concat [ps |> List.ofSeq; x.positions])

  //   member x.join (b:basin) =
  //     basin(List.concat [x.positions; b.positions])

  // let max (positions:pos seq) =
  //   let x = (positions |> Seq.maxBy (fun p -> p.x)).x
  //   let y = (positions |> Seq.maxBy (fun p -> p.y)).y
  //   { x = x; y = y; h = 0 }

  let rec basinNeighbors (positions:pos[]) (p:pos) =
      let bn =
        actualNeighbors positions p
        |> Array.filter (fun p -> p.h < 9)

      let remaining =
        positions
        |> Array.filter (fun q ->
          (q <> p) && ((Array.contains q bn) = false)
        )

      let otherNeighbors =
        bn
        |> Array.map (basinNeighbors remaining)
        |> Array.concat

      Array.concat [|[|p|]; bn; otherNeighbors|]
      |> Array.distinct

  let positionsToBasins (positions:pos[]) =
    lowPoints positions
    |> Array.map (fun l -> (l,(basinNeighbors positions l)))
    // |> Array.map (nn)
    // |> Array.map basin

  let multipleOfThreeLargestBasins (positions:pos[]) =
    positionsToBasins positions
    |> Array.map (fun (_,b) -> b.Length)
    |> Array.sortDescending
    |> Array.take 3
    |> Array.fold (fun runningTotal n -> runningTotal * n) 1

  [<Fact>]
  let ``Day 09 - test - positionsToBasins`` () =
    let positions = positionsFromInput day09sample

    let basins = positionsToBasins positions

    // |> Array.length
    // |> Array.take 1
    basins
    |> Array.map (fun (l,b) -> b.Length)
    |> should equal [|3; 9; 14; 9|]

    snd basins.[0]
    |> should equal [|
      { x = 1; y = 0; h = 1 };
      { x = 0; y = 0; h = 2 };
      { x = 0; y = 1; h = 3 }
    |]

  [<Fact>]
  let ``Day 09 - part 2 - sample`` () =
    let positions = positionsFromInput day09sample

    multipleOfThreeLargestBasins positions
    |> should equal 1134

  [<Fact(Skip="inefficient; ran for over an hour with no result")>]
  let ``Day 09 - part 2 - calculation`` () =
    let positions = positionsFromInput day09data

    multipleOfThreeLargestBasins positions
    |> should equal -1