namespace AdventOfCode.Tests

module Day06 =

  open AdventOfCode.Input
  open AdventOfCode.Lanternfish
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData(8L,0L)>]
  [<InlineData(7L,1L)>]
  [<InlineData(6L,2L)>]
  [<InlineData(5L,3L)>]
  [<InlineData(4L,4L)>]
  [<InlineData(3L,5L)>]
  [<InlineData(2L,6L)>]
  [<InlineData(1L,7L)>]
  [<InlineData(0L,8L)>]
  let ``Day 06 - age of a fish`` (daysUntilFirstReproduction:int64, expectedAge:int64) =

    Fish(daysUntilFirstReproduction).age
    |> should equal expectedAge

  [<Theory>]
  [<InlineData(3L, 0L, 0L)>]
  [<InlineData(3L, 1L, 0L)>]
  [<InlineData(3L, 2L, 0L)>]
  [<InlineData(3L, 3L, 0L)>]
  [<InlineData(3L, 4L, 1L)>]
  [<InlineData(3L, 5L, 1L)>]
  [<InlineData(3L, 6L, 1L)>]
  [<InlineData(3L, 7L, 1L)>]
  [<InlineData(3L, 8L, 1L)>]
  [<InlineData(3L, 9L, 1L)>]
  [<InlineData(3L,10L, 1L)>]
  [<InlineData(3L,11L, 2L)>]
  [<InlineData(3L,12L, 2L)>]
  let ``Day 06 - number of direct children after n days`` (r:int64, n:int64, c:int64) =

    Fish(r).numberOfChildrenAfterNDays n
    |> should equal c

  [<Theory>]
  [<InlineData(3L, 1L,  4L)>]
  [<InlineData(3L, 2L, 11L)>]
  [<InlineData(3L, 3L, 18L)>]
  let ``Day 06 - days to birth of child`` (r:int64, c:int64, d:int64) =

    Fish(r).daysToBirthOfChild c
    |> should equal d

  [<Theory>]
  [<InlineData(3L, 1L, 1L)>]
  [<InlineData(3L, 2L, 1L)>]
  [<InlineData(3L, 3L, 1L)>]
  [<InlineData(3L, 4L, 2L)>]
  [<InlineData(3L, 5L, 2L)>]
  [<InlineData(3L, 6L, 2L)>]
  [<InlineData(3L, 7L, 2L)>]
  [<InlineData(3L, 8L, 2L)>]
  [<InlineData(3L, 9L, 2L)>]
  [<InlineData(3L,10L, 2L)>]
  [<InlineData(3L,11L, 3L)>]
  [<InlineData(3L,12L, 3L)>]
  [<InlineData(3L,13L, 4L)>]
  [<InlineData(3L,14L, 4L)>]
  [<InlineData(3L,15L, 4L)>]
  [<InlineData(3L,16L, 4L)>]
  [<InlineData(3L,17L, 4L)>]
  [<InlineData(3L,18L, 5L)>]
  let ``Day 06 - size of family after n days`` (r:int64, n:int64, s:int64) =

    Fish(r).sizeOfFamilyAfterNDays n
    |> should equal s

  [<Fact>]
  let ``Day 06 - Part 1 - example`` () =
    let population =
      day06sample |> poplationOfLanternfishFromInput
    
    population
    |> Array.map (fun f -> f.sizeOfFamilyAfterNDays 80)
    |> Array.sum
    |> should equal ( 5934 |> int64 )

  [<Fact>]
  let ``Day 06 - Part 1 - calculation`` () =
    let population =
      day06data |> poplationOfLanternfishFromInput
    
    population
    |> Array.map (fun f -> f.sizeOfFamilyAfterNDays 80)
    |> Array.sum
    |> should equal ( 390923 |> int64 )

  [<Fact>]
  let ``Day 06 - Part 2 - example`` () =
    let population =
      day06sample |> poplationOfLanternfishFromInput
    
    population
    |> Array.map (fun f -> f.sizeOfFamilyAfterNDays 256)
    |> Array.sum
    |> should equal 26_984_457_539L
