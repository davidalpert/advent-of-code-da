module Tests

open AdventOfCode
open System
open Xunit
open FsUnit.Xunit

let soundingsFromInput (input:string) =
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())
    |> Seq.map (fun s -> s |> int)
    |> List.ofSeq

let numberOfTimesDepthIncreases (soundings:int list) = 
    let rec loop prev rest acc =
        match rest with
        | head :: tail ->
            if head > prev then
                loop head tail (acc+1)
            else
                loop head tail acc
        | [] -> acc

    match soundings with
    | head :: tail -> loop head tail 0
    | _ -> 0

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
