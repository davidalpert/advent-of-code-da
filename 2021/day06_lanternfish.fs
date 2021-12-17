namespace AdventOfCode

module Lanternfish =

  type PopulationModel(fishByDaysBeforeNextChild:array<int64>) =
    new(input:string) =
      let pop = Array.zeroCreate<int64> 9
      input.Trim().Split(",")
        |> Array.map int
        |> Seq.groupBy (fun d -> d)
        |> Seq.iter (fun (d,ff) -> 
          pop[d] <- (ff |> Seq.length |> int64)
        )
      PopulationModel(pop)

    member x.tick =
      let pop = Array.zeroCreate<int64> 9
      seq { 1 .. 8 }
      |> Seq.iter (fun r ->
        pop[r-1] <- fishByDaysBeforeNextChild[r]
      )
      pop[8] <- fishByDaysBeforeNextChild[0]
      pop[0] <- fishByDaysBeforeNextChild[1]
      pop[1] <- fishByDaysBeforeNextChild[2]
      pop[2] <- fishByDaysBeforeNextChild[3]
      pop[3] <- fishByDaysBeforeNextChild[4]
      pop[4] <- fishByDaysBeforeNextChild[5]
      pop[5] <- fishByDaysBeforeNextChild[6]
      pop[6] <- fishByDaysBeforeNextChild[7] + fishByDaysBeforeNextChild[0]
      pop[7] <- fishByDaysBeforeNextChild[8]
      PopulationModel(pop)

    member x.afterNDays (n:int) =
      if n < 1 then
        x
      else
        seq { 1 .. n }
        |> Seq.fold (fun p _ -> p.tick) x

    member x.totalSize =
      fishByDaysBeforeNextChild
      |> Array.sum

    member x.allFishByDaysToNextReproduction =
      fishByDaysBeforeNextChild
    