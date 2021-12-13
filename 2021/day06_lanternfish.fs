namespace AdventOfCode

module Lanternfish =

  open AdventOfCode.Input
  open System.Collections.Generic

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
    
    