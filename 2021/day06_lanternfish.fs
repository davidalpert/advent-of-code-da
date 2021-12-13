namespace AdventOfCode

module Lanternfish =

  open AdventOfCode.Input
  open System.Collections.Generic

  type PopulationModel(ageInDays:int64, initialPopulation:int64 list) =
    let population = initialPopulation

    member x.ageInDays =
      ageInDays
    
    member x.size =
      population.Length |> int64

    member x.tick =
      let babies = new List<int64>()
      let oldFish =
        population
        |> List.map (fun a ->
          match a with
          | 0L ->
            babies.Add(8)
            6L
          | _ -> a-1L
        )
      let newFish = babies |> List.ofSeq
      let newPop = List.concat [oldFish;newFish]
      let newAgeInDays = ageInDays + 1L

      // printfn "After %d days: %A" newAgeInDays newPop

      PopulationModel(newAgeInDays, newPop)
    
    member x.projectToDay(projectedAge:int) =
      let ageOneDay (model:PopulationModel) (newAge:int64) =
        model.tick

      let newAge = (projectedAge |> int64) - 1L
      if newAge < x.ageInDays then
        x
      else
        seq { x.ageInDays .. newAge }
        |> Seq.fold ageOneDay x

  let toPopulationModel(ages:string[]) =
    let population =
      ages
      |> Array.map (fun a -> a |> int64)
      |> List.ofArray

    PopulationModel(0, population)

  let populationModelFromInput (input:string) =
    input.Trim().Split(",")
    |> toPopulationModel
    
    