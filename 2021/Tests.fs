module Tests

open AdventOfCode
open System
open Xunit
open FsUnit.Xunit

let soundingsFromInput (input:string) =
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())
    |> Seq.map (fun s -> s |> int)

let submarineInstructionsFromInput (input:string) =
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())
    |> Seq.map (fun s -> s.Split " ")
    |> Seq.map List.ofArray

let depthIncreased (prev,next) =
    next > prev

let numberOfTimesDepthIncreases (soundings:int seq) = 
    soundings
    |> Seq.pairwise
    |> Seq.where depthIncreased
    |> Seq.length

[<Fact>]
let ``Day 01 - Part 1 - Example`` () =
    Input.day01sample
    |> soundingsFromInput
    |> numberOfTimesDepthIncreases
    |> should equal 7

[<Fact>]
let ``Day 01 - Part 1 - Calculation`` () =
    Input.day01
    |> soundingsFromInput
    |> numberOfTimesDepthIncreases
    |> should equal 1215

[<Fact>]
let ``Day 01 - Part 2 - Example`` () =
    Input.day01sample
    |> soundingsFromInput
    |> Seq.windowed 3
    |> Seq.map Seq.sum 
    |> numberOfTimesDepthIncreases
    |> should equal 5

[<Fact>]
let ``Day 01 - Part 2 - Calculation`` () =
    Input.day01
    |> soundingsFromInput
    |> Seq.windowed 3
    |> Seq.map Seq.sum 
    |> numberOfTimesDepthIncreases
    |> should equal 1150

type SubmarinePosition = {
    horizontal : int
    depth : int
}
with
    member x.displacement =
        x.horizontal * x.depth

let initialPosition = {
    horizontal = 0;
    depth = 0;
}

let navigateSubmarine (startingPosition:SubmarinePosition) (instructions:string list) =
    match instructions with
    | "forward" :: [x] -> { startingPosition with horizontal = startingPosition.horizontal + (x |> int) }
    | "up"      :: [x] -> { startingPosition with depth = startingPosition.depth - (x |> int) }
    | "down"    :: [x] -> { startingPosition with depth = startingPosition.depth + (x |> int) }
    | _ -> startingPosition

[<Fact>]
let ``Day 02 - Part 1 - Example`` () =
    Input.day02sample
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarine initialPosition
    |> (fun p -> p.displacement)
    |> should equal 150

[<Fact>]
let ``Day 02 - Part 1 - Calculation`` () =
    Input.day02
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarine initialPosition
    |> (fun p -> p.displacement)
    |> should equal 2073315