namespace AdventOfCode.Tests

module Day06 =

  open AdventOfCode.Input
  open AdventOfCode.Lanternfish
  open Xunit
  open FsUnit.Xunit

  [<Fact>]
  let ``Day 06 - test`` () =
    let m =
      day06sample
      |> PopulationModel

    m.allFishByDaysToNextReproduction
    //                0   1   2   3   4   5   6   7   8
    |> should equal [|0L; 1L; 1L; 2L; 1L; 0L; 0L; 0L; 0L|]

    m.tick.allFishByDaysToNextReproduction
    //                0   1   2   3   4   5   6   7   8
    |> should equal [|1L; 1L; 2L; 1L; 0L; 0L; 0L; 0L; 0L|]

    m.tick.tick.allFishByDaysToNextReproduction
    //                0   1   2   3   4   5   6   7   8
    |> should equal [|1L; 2L; 1L; 0L; 0L; 0L; 1L; 0L; 1L|]

    (m.afterNDays 0).allFishByDaysToNextReproduction
    |> should equal m.allFishByDaysToNextReproduction

    (m.afterNDays 1).allFishByDaysToNextReproduction
    |> should equal m.tick.allFishByDaysToNextReproduction

    (m.afterNDays 2).allFishByDaysToNextReproduction
    |> should equal m.tick.tick.allFishByDaysToNextReproduction

  [<Fact>]
  let ``Day 06 - Part 1 - example`` () =
    let m = day06sample |> PopulationModel
    (m.afterNDays 80).totalSize
    |> should equal ( 5934 |> int64 )

  [<Fact>]
  let ``Day 06 - Part 1 - calculation`` () =
    let m = day06data |> PopulationModel
    
    (m.afterNDays 80).totalSize
    |> should equal ( 390923 |> int64 )

  [<Fact>]
  let ``Day 06 - Part 2 - example`` () =
    let m = day06sample |> PopulationModel
    (m.afterNDays 256).totalSize
    |> should equal 26_984_457_539L

  [<Fact>]
  let ``Day 06 - Part 2 - calculation`` () =
    let m = day06data |> PopulationModel
    (m.afterNDays 256).totalSize
    |> should equal 1749945484935L
