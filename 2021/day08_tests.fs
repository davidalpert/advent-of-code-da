namespace AdventOfCode.Tests

module Day08 =

  open AdventOfCode.Input
  open Xunit
  open FsUnit.Xunit

  let digitFromSignals (s:string) =
    match s.Length with
    | 2 -> Some(1)
    | 4 -> Some(4)
    | 3 -> Some(7)
    | 7 -> Some(8)
    | _ -> None
    
  let isSimpleNumber (s:string) =
    match s.Length with
    | 2 -> true
    | 4 -> true
    | 3 -> true
    | 7 -> true
    | _ -> false

  let numberOfSimpleDigitAnswersInLine (lineOfInput:string) =
    // printfn "processing: '%s'" lineOfInput
    let parts = lineOfInput.Split("|")
    let right = parts.[1].Split(" ") |> Array.map (fun s -> s.Trim())

    right
    |> Array.filter isSimpleNumber
    |> Array.length

  let numberOfSimpleDigitAnswersInInput (input:string) =
    input.Trim().Split("\n")
    |> Array.map numberOfSimpleDigitAnswersInLine
    |> Array.sum

  [<Theory>]
  [<InlineData("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
fdgacbe cefdb cefbgd gcbe", 2)>]
  let ``Day 08 - test - line of input - simple digits`` (lineOfInput:string, numberOfSimpleDigits:int) =

    numberOfSimpleDigitAnswersInLine lineOfInput
    |> should equal numberOfSimpleDigits

  [<Fact>]
  let ``Day 08 - part 1 - example`` () =
    numberOfSimpleDigitAnswersInInput day08sample
    |> should equal 26

  [<Fact>]
  let ``Day 08 - part 1 - calculation`` () =
    numberOfSimpleDigitAnswersInInput day08data
    |> should equal 521


    

