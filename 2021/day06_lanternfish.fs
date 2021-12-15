namespace AdventOfCode

module Lanternfish =

  open AdventOfCode.Input
  open System.Collections.Generic

  type Fish(r:int64) =
    member f.age =
      8L - r

    member f.numberOfChildrenAfterNDays (n:int64) =
      let numberOfReproductiveDays = ((f.age - 2L) + n)
      numberOfReproductiveDays / 7L

    member f.daysToBirthOfChild (i:int64) =
      (i - 1L) * 7L + (r + 1L)

    member f.sizeOfFamilyAfterNDays (n:int64) =
      if n < 0 then
        0L
      else
        let cc = f.numberOfChildrenAfterNDays n

        let d =
          seq { 1L .. cc }
          |> Seq.map (fun i ->
            // printfn "after %d days; child %d" n i
            if n = 256 then
              printfn "child %d" i
            else
              ()

            let d = f.daysToBirthOfChild i
            Fish().sizeOfFamilyAfterNDays (n - d)
          )
          |> Seq.sum
        
        d + 1L

    new() = Fish(8) // default r

  let poplationOfLanternfishFromInput (input:string) =
    input.Trim().Split(",")
    |> Array.map int64
    |> Array.map Fish

  // type Fish2(daysToFirstReproduction:int, dayOfBirth:int) =
  //   let ageOfDay0 = 8 - daysToFirstReproduction

  //   member x.dayOfFirstChild =
  //     daysToFirstReproduction + 1

  //   member x.ageAfterNDays (n:int) =
  //     ageOfDay0 + n |> int64

  //   member x.daysToNextReproductionAfterNDays (n:int) =
  //     6L - (((x.ageAfterNDays n) - 2L) % 7L)

  //   member x.numberOfDirectChildrenAfterNDays (n:int) =
  //     //=floor(divide(AC7-2,7))
  //     ((x.ageAfterNDays n) - 2L) / 7L

  //   member x.daysLeftForNthChildUntilM (n:int) (m:int) =
  //       // m - (x.dayOfFirstChild + ((n-1) * 7))
  //       0


  //   member x.sizeOfFamilyAfterNDays (n:int) =
  //     if n < 0 then
  //       0L
  //     else 
  //       // let daysLeftForMthChild (birthOrder:int) =
  //         // n - (x.dayOfFirstChild + ((birthOrder-1) * 7))

  //       // let numberInNextGen (daysLeft:int) =
  //       //   let f = Fish2 8
  //       //   (f.numberOfDescendentsAfterNDays daysLeft) + 1

  //       let d = x.numberOfDirectChildrenAfterNDays(n)

  //       let m = 
  //         seq { 1L .. d }
  //         |> Seq.map (fun i -> 
  //           let daysRemaining = (n |> int64) - ((i-1L) * 7L)
  //           let f = Fish2 6
  //           f.sizeOfFamilyAfterNDays (daysRemaining - 2)
  //         )
  //         |> Seq.sum

  //       d + m + 1L // and this one

  //       // |> Seq.map numberInNextGen
  //       // |> Seq.fold (fun total n -> total + n) 0
  
  //   // member x.familyAfterNDays(n:int) =
  //   //   let initialAge = 8 - daysToFirstReproduction
  //   //   let finalAge = initialAge + n
  //   //   let numberOfChildren = (finalAge - 2) / 6
      
  //   new(daysToFirstReproduction:int) =
  //     Fish2(daysToFirstReproduction,0)



  type PopulationModel(ageInDays:int, initialPopulation:int list) =
    let population = initialPopulation

    member x.ageInDays =
      ageInDays
    
    member x.size =
      population.Length

    member x.tick =
      let babies = new List<int>()
      let oldFish =
        population
        |> List.map (fun a ->
          match a with
          | 0 ->
            babies.Add(8)
            6
          | _ -> a-1
        )
      let newFish = babies |> List.ofSeq
      let newPop = List.concat [oldFish;newFish]
      let newAgeInDays = ageInDays + 1

      // printfn "After %d days: %A" newAgeInDays newPop

      PopulationModel(newAgeInDays, newPop)
    
    member x.projectToDay(projectedAge:int) =
      let ageOneDay (model:PopulationModel) (newAge:int) =
        model.tick

      let newAge = projectedAge - 1
      if newAge < x.ageInDays then
        x
      else
        seq { x.ageInDays .. newAge }
        |> Seq.fold ageOneDay x

  let toPopulationModel(ages:string[]) =
    let population =
      ages
      |> Array.map (fun a -> a |> int)
      |> List.ofArray

    PopulationModel(0, population)

  let populationModelFromInput (input:string) =
    input.Trim().Split(",")
    |> toPopulationModel
    
    