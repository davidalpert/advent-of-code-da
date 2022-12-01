namespace AdventOfCode

module Day21 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.RPGSimulator20XX
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
"""

    let puzzleInput =
        { HitPoints = 100
          Damage = 8
          Armor = 2 }

    [<Theory>]
    [<InlineData(8, 3, 5)>]
    [<InlineData(8, 300, 1)>]
    let ``test`` (attackerDamage, defenderArmor, expectedDamage) =
        let player =
            { HitPoints = 0
              Damage = attackerDamage
              Armor = 0 }

        let other =
            { HitPoints = 0
              Damage = 0
              Armor = defenderArmor }

        player.attack other |> should equal expectedDamage

    [<Fact>]
    let ``2015 - Day 21 - example`` () =
        availablePurchases |> Seq.length |> should equal 0

// // [<Fact>]
// let ``2015 - Day 21 - part 1`` () =
//     puzzleInput
//     |> fromInput
//     |> Array.length
//     |> should equal 0
