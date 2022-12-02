namespace AdventOfCode

module CalorieCounting =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    let inputToElvesWithCalories (s:string) =
        s.Trim().Split("\n\n")
        |> Array.map (fun s -> s.Split("\n"))
        |> Array.map (fun ss -> ss
                             |> Array.map (fun s -> s |> int))
        |> Array.mapi (fun i items ->
            let n = i + 1 // arrays are 0-indexed; humans count from 1
            let calories =
                items
                |> Array.sum
            (n, calories))
        
    let inputToElfWithMaxCalories (s: string) =
        s
        |> inputToElvesWithCalories
        |> Array.maxBy snd
    
    let inputToTopThreeElvesCarryingTheMostCalories (s: string) =
        s
        |> inputToElvesWithCalories
        |> Array.sortByDescending snd
        |> Array.take 3
    
