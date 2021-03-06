namespace AdventOfCode

module ScienceForHungryPeople =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    type Ingredient =
        { name: string
          capacity: int
          durability: int
          flavor: int
          texture: int
          calories: int }

    type CookieRecipe(allIngredients: Ingredient [], portions: int []) =
        member this.ingredients =
            portions
            |> Array.mapi (fun i n -> (n, allIngredients.[i]))

        member this.totals =
            // - A capacity of 44*-1 + 56*2 = 68
            // - A durability of 44*-2 + 56*3 = 80
            // - A flavor of 44*6 + 56*-2 = 152
            // - A texture of 44*3 + 56*-1 = 76
            // Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now) results in a total score

            let initialValues =
                { name = "total"
                  capacity = 0
                  durability = 0
                  flavor = 0
                  texture = 0
                  calories = 0 }

            let folder (acc: Ingredient) (n: int, i: Ingredient) =
                { acc with
                    capacity = acc.capacity + n * i.capacity
                    durability = acc.durability + n * i.durability
                    flavor = acc.flavor + n * i.flavor
                    texture = acc.texture + n * i.texture
                    calories = acc.calories + n * i.calories }

            this.ingredients
            |> Array.fold folder initialValues

        member this.totalScore =
            let multiplyBy x y = x * y

            [ Math.Max(0, this.totals.capacity)
              Math.Max(0, this.totals.durability)
              Math.Max(0, this.totals.flavor)
              Math.Max(0, this.totals.texture) ]
            |> Seq.fold multiplyBy 1

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Seq.map (fun ss ->
            match ss with
            | Regex @"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)"
                    [ name; cap; dur; flav; text; cal ] ->
                { name = name
                  capacity = (cap |> int)
                  durability = (dur |> int)
                  flavor = (flav |> int)
                  texture = (text |> int)
                  calories = (cal |> int) }
            | _ -> raise (System.Exception(sprintf "could not parse: '%s'" ss)))
        |> Array.ofSeq

    let recipeCombinationsOfN (n: int) (k: int) =
        seq { 1..n }
        |> List.ofSeq
        |> List.map (fun i -> seq { 0..k } |> List.ofSeq)
        |> cart1
        |> Seq.where (fun l -> l |> List.sum = k)
        |> Array.ofSeq
        |> Array.map (fun l -> l |> Array.ofList)

    let generateAllPossibleCookieRecipies (nTeaspoons: int) (input: string) =
        let allIngredients = input |> fromInput

        let allCombinations = recipeCombinationsOfN (allIngredients.Length) nTeaspoons

        allCombinations
        |> Array.map (fun ii -> CookieRecipe(allIngredients, ii))
        |> Array.map (fun c -> (c, c.totalScore))

    let findHighestScoringCookieRecipe (nTeaspoons: int) (input: string) =
        input
        |> generateAllPossibleCookieRecipies nTeaspoons
        |> Array.maxBy snd

    let findHighestScoringCookieRecipeWithCalorieLimit (nTeaspoons: int) (exactCalories: int) (input: string) =
        input
        |> generateAllPossibleCookieRecipies nTeaspoons
        |> Array.filter (fun (c, _) -> c.totals.calories = exactCalories)
        |> Array.maxBy snd
