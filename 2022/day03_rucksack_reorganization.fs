namespace AdventOfCode

module RucksackReorganization =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input

    let asPriority (c:char) =
        match c |> Char.IsLower with
        | true -> int c - int 'a' + 1
        | false -> int c - int 'A' + 27
    
    // type Rucksack = Compartment * Compartment
    type Rucksack(s:string) =
        let contents = s.ToCharArray()
        let midpoint = contents.Length / 2
        
        member this.compartments =
            (
                contents |> Array.take (midpoint),
                contents |> Array.skip (midpoint)
            )
            
        member this.itemsInBoth =
            let first = this.compartments |> fst |> set
            let second = this.compartments |> snd |> set
            Set.intersect first second
            
    let prioritiesOfItemsInBothCompartments (r: Rucksack): int[] =
        r.itemsInBoth
        |> Array.ofSeq
        |> Array.map asPriority
       
    let sumOfPrioritiesOfItemsInBothCompartments (r: Rucksack): int =
        r |> prioritiesOfItemsInBothCompartments |> Array.sum
        
    let toRucksacks (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            Rucksack(s))
        |> Array.ofSeq
        
    let part1_find_the_sum_of_the_priorities_of_the_items_in_both_rucksacks (input: string) =
        input
        |> toRucksacks
        |> Array.map sumOfPrioritiesOfItemsInBothCompartments
        |> Array.sum
