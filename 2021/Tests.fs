module Tests

open AdventOfCode
open System
open Xunit
open FsUnit.Xunit

let soundingsFromInput (input:string) =
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())
    |> Seq.map (fun s -> s |> int)

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

