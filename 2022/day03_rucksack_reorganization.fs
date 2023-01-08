namespace AdventOfCode

module RucksackReorganization =

    open System
    open System.Collections.Generic
    open AdventOfCode.utils

    let asPriority (c:char) =
        match c |> Char.IsLower with
        | true -> int c - int 'a' + 1
        | false -> int c - int 'A' + 27
    
    // type Rucksack = Compartment * Compartment
    type Rucksack(s:string) =
        let halves = s.ToCharArray() |> Array.splitInto 2
        member this.compartments =
            (
                halves[0],
                halves[1]
            )
            
        member this.itemsInBoth =
            let (first, second) = this.compartments
            Set.intersect (first |> set) (second |> set)
            
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
        
    let part2_find_the_sum_of_the_priorities_of_the_badges_for_each_three_elf_group (input: string) =
        input
        |> splitToTrimmedLines
        |> Seq.chunkBySize 3
        |> Seq.map (fun s ->
               s
               |> Array.map set
               |> Set.intersectMany
               |> Seq.map asPriority
               |> Seq.sum
            )
        |> Seq.sum
 