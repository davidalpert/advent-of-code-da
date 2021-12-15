namespace AdventOfCode

module Lanternfish =

  open AdventOfCode.Input
  open System.Collections.Generic

  type Fish(daysToFirstReproduction:int, dayOfBirth:int) =
    let ageOfDay0 = 8 - daysToFirstReproduction

    member x.dayOfFirstChild =
      daysToFirstReproduction + 1

    member x.ageAfterNDays (n:int) =
      ageOfDay0 + n

    member x.daysToNextReproductionAfterNDays (n:int) =
      6 - (((x.ageAfterNDays n) - 2) % 7)

    member x.numberOfDirectChildrenAfterNDays (n:int) =
      //=floor(divide(AC7-2,7))
      ((x.ageAfterNDays n) - 2) / 7

    member x.daysLeftForNthChildUntilM (n:int) (m:int) =
        // m - (x.dayOfFirstChild + ((n-1) * 7))
        0


    member x.sizeOfFamilyAfterNDays (n:int) =
      if n < 0 then
        0L
      else 
        // let daysLeftForMthChild (birthOrder:int) =
          // n - (x.dayOfFirstChild + ((birthOrder-1) * 7))

        // let numberInNextGen (daysLeft:int) =
        //   let f = Fish 8
        //   (f.numberOfDescendentsAfterNDays daysLeft) + 1

        let m = 
          seq { 1 .. x.numberOfDirectChildrenAfterNDays(n) }
          |> Seq.map (fun i -> 
            let daysRemaining = n - ((i-1) * 7) 
            let f = Fish 6
            f.sizeOfFamilyAfterNDays (daysRemaining - 2)
          )
          |> Seq.sum

        m + 1L // and this one

        // |> Seq.map numberInNextGen
        // |> Seq.fold (fun total n -> total + n) 0
  
    // member x.familyAfterNDays(n:int) =
    //   let initialAge = 8 - daysToFirstReproduction
    //   let finalAge = initialAge + n
    //   let numberOfChildren = (finalAge - 2) / 6
      
    new(daysToFirstReproduction:int) =
      Fish(daysToFirstReproduction,0)



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
    
    