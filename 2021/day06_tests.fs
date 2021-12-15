namespace AdventOfCode.Tests

module Day06 =

  open AdventOfCode.Input
  open AdventOfCode.Lanternfish
  open Xunit
  open FsUnit.Xunit

  [<Fact>]
  let ``Day 06 - Part 1 - example`` () =
    let model =
      day06sample |> populationModelFromInput
    
    let today = model.projectToDay 80

    today.size
    |> should equal 5934

  [<Fact>]
  let ``Day 06 - Part 1 - calculation`` () =
    let model =
      day06data |> populationModelFromInput
    
    let today = model.projectToDay 80

    today.size
    |> should equal 390923

  [<Theory>]
  [<InlineData(8,0)>]
  [<InlineData(7,1)>]
  [<InlineData(6,2)>]
  [<InlineData(5,3)>]
  [<InlineData(4,4)>]
  [<InlineData(3,5)>]
  [<InlineData(2,6)>]
  [<InlineData(1,7)>]
  [<InlineData(0,8)>]
  let ``Day 06 - Part 2 - initial age of a fish`` (initialDaysToReproduce:int,initialAge:int) =

    let f = Fish initialDaysToReproduce

    f.ageAfterNDays 0
    |> should equal initialAge
    
  [<Theory>]
  [<InlineData(8,1,1)>]
  [<InlineData(7,1,2)>]
  [<InlineData(6,1,3)>]
  [<InlineData(5,1,4)>]
  [<InlineData(4,1,5)>]
  [<InlineData(3,1,6)>]
  [<InlineData(2,1,7)>]
  [<InlineData(1,1,8)>]
  [<InlineData(0,1,9)>]
  [<InlineData(0,2,10)>]
  [<InlineData(0,3,11)>]
  let ``Day 06 - Part 2 - age after n days`` (initialDaysToReproduce:int,nDays:int,eventualAge:int) =

    let f = Fish initialDaysToReproduce

    f.ageAfterNDays nDays
    |> should equal eventualAge
    
  [<Theory>]
  [<InlineData(8,0,8)>]
  [<InlineData(8,1,7)>]
  [<InlineData(8,2,6)>]
  [<InlineData(8,3,5)>]
  [<InlineData(8,4,4)>]
  [<InlineData(8,5,3)>]
  [<InlineData(8,6,2)>]
  [<InlineData(8,7,1)>]
  [<InlineData(8,8,0)>]
  [<InlineData(3,1,2)>]
  [<InlineData(3,2,1)>]
  [<InlineData(3,3,0)>]
  [<InlineData(3,4,6)>]
  [<InlineData(3,17,0)>]
  [<InlineData(3,18,6)>]
  let ``Day 06 - Part 2 - days to next repro after n days`` (initialDaysToReproduce:int,nDays:int,daysToReproduceAfterNDays:int) =

    let f = Fish initialDaysToReproduce

    f.daysToNextReproductionAfterNDays nDays
    |> should equal daysToReproduceAfterNDays
    
  [<Theory>]
  [<InlineData(3,0,0)>]
  [<InlineData(3,1,0)>]
  [<InlineData(3,2,0)>]
  [<InlineData(3,3,0)>]
  [<InlineData(3,4,1)>]
  [<InlineData(3,5,1)>]
  [<InlineData(3,6,1)>]
  [<InlineData(3,7,1)>]
  [<InlineData(3,8,1)>]
  [<InlineData(3,9,1)>]
  [<InlineData(3,10,1)>]
  [<InlineData(3,11,2)>]
  [<InlineData(3,12,2)>]
  [<InlineData(3,13,3)>]
  [<InlineData(3,14,3)>]
  [<InlineData(3,15,3)>]
  [<InlineData(3,16,3)>]
  [<InlineData(3,17,3)>]
  [<InlineData(3,18,4)>]
  let ``Day 06 - Part 2 - number of children after n days`` (initialDaysToReproduce:int,nDays:int,expectedNumberOfChildrenAfterNDays:int) =

    let f = Fish initialDaysToReproduce

    f.numberOfDirectChildrenAfterNDays nDays
    |> should equal expectedNumberOfChildrenAfterNDays
    
  [<Theory>]
  [<InlineData(3,4)>]
  [<InlineData(4,5)>]
  [<InlineData(1,2)>]
  [<InlineData(2,3)>]
  let ``Day 06 - Part 2 - day of first reproduction`` (initialDaysToReproduce:int,dayOfFirstChild:int) =
    let f = Fish initialDaysToReproduce

    f.dayOfFirstChild
    |> should equal dayOfFirstChild

  [<Theory>]
  [<InlineData(3,0,0,0)>]
  // [<InlineData(3,4,1,0)>]
  // [<InlineData(3,4,1,1)>]
  let ``Day 06 - Part 2 - number of ndays left for nth child`` (initialDaysToReproduce:int,nDays:int,nthChild:int,expectedDaysLeft:int) =
    let f = Fish initialDaysToReproduce

    f.daysLeftForNthChildUntilM nthChild nDays 
    |> should equal expectedDaysLeft
    
  [<Theory>]
  [<InlineData(3, 0,1)>]
  [<InlineData(3, 1,1)>]
  [<InlineData(3, 2,1)>]
  [<InlineData(3, 3,1)>]
  [<InlineData(3, 4,2)>]
  [<InlineData(3, 5,2)>]
  [<InlineData(3, 6,2)>]
  [<InlineData(3, 7,2)>]
  [<InlineData(3, 8,2)>]
  // [<InlineData(3, 9,2)>]
  // [<InlineData(3,10,2)>]
  // [<InlineData(3,11,3)>]
  // [<InlineData(3,12,3)>]
  // [<InlineData(3,13,4)>]
  // [<InlineData(3,14,4)>]
  // [<InlineData(3,15,4)>]
  // [<InlineData(3,16,4)>]
  // [<InlineData(3,17,4)>]
  // [<InlineData(3,18,5)>]
  let ``Day 06 - Part 2 - number of descendents after n days`` (initialDaysToReproduce:int,nDays:int,expectedNumberOfDescendents) =

    let f = Fish initialDaysToReproduce

    f.sizeOfFamilyAfterNDays nDays
    |> should equal ( expectedNumberOfDescendents |> int64 )


  // [<Theory>]
  // [<InlineData(3,5,1,2)>]
  // // [<InlineData(3,3,0)>]
  // // [<InlineData(3,4,6)>]
  // // [<InlineData(3,17,3)>]
  // let ``Day 06 - Part 2 - number of children`` (daysToFirstReproduction:int,initialAge, nDays:int,finalNumberOfDaysRemaining:int) =

  //   let f = Fish daysToFirstReproduction

  //   f.ageAfterNDays 0
  //   |> should equal 5

    // f.daysToNextReproduction nDays
    // |> should equal finalNumberOfDaysRemaining

    // // d:   3 2 1 0 6  5
    // // a:   5 6 7 8 9 10
    // // a-2: 3 4 5 6 7  8
    // // %    3 4 5 6 0  1      
    // // 6-n  3 2 1 0 6
    // let ageFromDaysToNextReproduction d =
    //   (6 - d) + 2

    // let ageNDaysFromNow a n =
    //   a + n

    // let daysToNextReproductionFromAge a =
    //   (a - 2)

    // let initialAge = ageFromDaysToNextReproduction daysToFirstReproduction

    // daysToNextReproductionFromAge (ageNDaysFromNow initialAge nDays)
    // |> should equal finalNumberOfDaysRemaining

    // let initialAge = 8 - daysToFirstReproduction
    // let finalAge = initialAge + nDays - 1
    // // let finalNumberOfDaysRemaining = (finalAge - 2)
    // let numberOfChildren = (finalAge - 2) / 7
    // numberOfChildren |> should equal expectedNumberOfChildren

  // [<Theory>]
  // [<InlineData(3,1,2)>]
  // [<InlineData(3,2,1)>]
  // [<InlineData(3,3,0)>]
  // [<InlineData(3,4,6)>]
  // let ``Day 06 - Part 2 - modulus`` (initialAge:int,nDays:int,expectedAge:int) =
  //                     v
  //   // Initial state: 3,
  //   // After  1 day:  2,
  //   // After  2 days: 1,
  //   // After  3 days: 0,
  //   // After  4 days: 6,8
  //   // After  5 days: 5,7,
  //   // After  6 days: 4,6,
  //   // After  7 days: 3,5,
  //   // After  8 days: 2,4,
  //   // After  9 days: 1,3,
  //   // After 10 days: 0,2,
  //   // After 11 days: 6,1,8
  //   // After 12 days: 5,0,7
  //   // After 13 days: 4,6,6,8
  //   // After 14 days: 3,5,5,7,
  //   // After 15 days: 2,4,4,6,
  //   // After 16 days: 1,3,3,5,
  //   // After 17 days: 0,2,2,4,
  //   // After 18 days: 6,1,1,3,8
  //                     v
  //   // Initial state: 3,4,3,1,2
  //   // After  1 day:  2,3,2,0,1
  //   // After  2 days: 1,2,1,6,0,8
  //   // After  3 days: 0,1,0,5,6,7,8   v
  //   // After  4 days: 6,0,6,4,5,6,7,8,8
  //   // After  5 days: 5,6,5,3,4,5,6,7,7,8
  //   // After  6 days: 4,5,4,2,3,4,5,6,6,7
  //   // After  7 days: 3,4,3,1,2,3,4,5,5,6
  //   // After  8 days: 2,3,2,0,1,2,3,4,4,5
  //   // After  9 days: 1,2,1,6,0,1,2,3,3,4,8
  //   // After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8 v
  //   // After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
  //   // After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8   v
  //   // After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
  //   // After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
  //   // After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
  //   // After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
  //   // After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8 v
  //   // After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8

  //   let daysLeft(age:int) =
  //     age % 7

  //   let ageAfterNDays (a:int) (nDays:int) =
  //     // n: 0 1 2 3 4 5 6 7
  //     // a: 6 5 4 3 2 1 0 6
  //     6 - (((6-a) + nDays) % 7)

  //   ageAfterNDays initialAge nDays
  //   |> should equal expectedAge


  // [<Fact>]
  // let ``Day 06 - Part 2 - example`` () =
  //   let model =
  //     day06sample |> populationModelFromInput
    
  //   let today = model.projectToDay 256

  //   today.size
  //   |> should equal 26_984_457_539L
