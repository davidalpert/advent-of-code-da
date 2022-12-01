namespace AdventOfCode

module RPGSimulator20XX =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    type Character =
        { HitPoints: int
          Damage: int
          Armor: int }
        member this.attack(other: Character) = Math.Max(this.Damage - other.Armor, 1)

    type Winner =
        | Boss of int // remaining hp
        | Player of int // remaining hp
        | Draw // both are 0 hp
        | None // both are + hp

    type Match =
        { Player: Character
          Boss: Character }
        member this.playerDoes = this.Player.attack (this.Boss)
        member this.bossDoes = this.Boss.attack (this.Player)
        member this.damagePerRound = (this.playerDoes, this.bossDoes)

        member this.play() =
            let bossDamage = this.Player.attack (this.Boss)
            None

        member this.winner =
            match (this.Player.HitPoints, this.Boss.HitPoints) with
            | (0, 0) -> Draw
            | (0, _) -> Boss(this.Boss.HitPoints)
            | (_, 0) -> Player(this.Player.HitPoints)
            | (_, _) -> None

    type Item =
        { Name: string
          Damage: int
          Armor: int
          Cost: int }

    let availableWeapons =
        [ { Name = "Dagger"
            Cost = 8
            Damage = 4
            Armor = 0 }
          { Name = "Shortsword"
            Cost = 10
            Damage = 5
            Armor = 0 }
          { Name = "Warhammer"
            Cost = 25
            Damage = 6
            Armor = 0 }
          { Name = "Longsword"
            Cost = 40
            Damage = 7
            Armor = 0 }
          { Name = "Greataxe"
            Cost = 74
            Damage = 8
            Armor = 0 } ]

    let availableArmor =
        [ { Name = "None"
            Cost = 0
            Damage = 0
            Armor = 0 }
          { Name = "Leather"
            Cost = 13
            Damage = 0
            Armor = 1 }
          { Name = "Chainmail"
            Cost = 31
            Damage = 0
            Armor = 2 }
          { Name = "Splitmail"
            Cost = 53
            Damage = 0
            Armor = 3 }
          { Name = "Bandedmail"
            Cost = 75
            Damage = 0
            Armor = 4 }
          { Name = "Platemail"
            Cost = 102
            Damage = 0
            Armor = 5 } ]

    let availableRings =
        [ { Name = "None" // (left)
            Cost = 0
            Damage = 0
            Armor = 0 }
          { Name = "None" // (right)
            Cost = 0
            Damage = 0
            Armor = 0 }
          { Name = "Damage +1"
            Cost = 25
            Damage = 1
            Armor = 0 }
          { Name = "Damage +2"
            Cost = 50
            Damage = 2
            Armor = 0 }
          { Name = "Damage +3"
            Cost = 100
            Damage = 3
            Armor = 0 }
          { Name = "Defense +1"
            Cost = 20
            Damage = 0
            Armor = 1 }
          { Name = "Defense +2"
            Cost = 40
            Damage = 0
            Armor = 2 }
          { Name = "Defense +3"
            Cost = 80
            Damage = 0
            Armor = 3 } ]

    let ringCombos =
        availableRings
        |> Array.ofList
        |> n_choose_k 2
        |> List.map (fun hands -> (hands |> List.head, hands |> List.tail |> List.head))

    type Equipment =
        | Weapon of Item
        | Armor of Item
        | Rings of Item * Item
        member this.ArmorScore =
            match this with
            | Weapon (i) -> i.Armor
            | Armor (i) -> i.Armor
            | Rings (left, right) -> left.Armor + right.Armor

        member this.DamageScore =
            match this with
            | Weapon (i) -> i.Damage
            | Armor (i) -> i.Damage
            | Rings (left, right) -> left.Damage + right.Damage

        member this.Cost =
            match this with
            | Weapon (i) -> i.Cost
            | Armor (i) -> i.Cost
            | Rings (left, right) -> left.Cost + right.Cost

    let availablePurchases =
        [ availableWeapons |> List.map (fun i -> Weapon(i))
          availableArmor |> List.map (fun i -> Armor(i))
          ringCombos |> List.map (fun (l, r) -> Rings(l, r)) ]
        |> cart1
