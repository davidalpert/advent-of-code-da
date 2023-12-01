namespace AdventOfCode

open System
open Input

module Calibration =
    let valueForLine (s:string) =
        let digits =
            s.ToCharArray()
            |> Array.filter Char.IsNumber
            
        let firstDigit = digits |> Array.head
        let lastDigit = digits |> Array.rev |> Array.head
        
        [|firstDigit; lastDigit|]
        |> String
        |> Int32.Parse
        
    let valueForInput (s:string) =
        s
        |> splitToTrimmedLines
        |> Seq.sumBy (fun line ->
                line |> valueForLine
            )

    let numberWordValues = dict [
        ("one", "1");
        ("two", "2");
        ("three", "3");
        ("four", "4");
        ("five", "5");
        ("six", "6");
        ("seven", "7");
        ("eight", "8");
        ("nine", "9");
    ]
    
    let preProcessLine (s:string) =
        
        
        let preProcessedString =
            numberWordValues.Keys
            |> Seq.fold (fun (state:string) word ->
                    state.Replace(word, numberWordValues[word])
                ) s
            
        preProcessedString
    
    let valueForLine2 (s:string) =
        let preProcessedString =
            numberWordValues.Keys
            |> Seq.fold (fun (state:string) word ->
                    state.Replace(word, numberWordValues[word])
                ) s
        
        // printf $"%s{s}\n"
        // printf $"%s{preProcessedString}\n"
        
        let digits =
            (s |> preProcessLine).ToCharArray()
            |> Array.filter Char.IsNumber
            
        let firstDigit = digits |> Array.head
        let lastDigit = digits |> Array.rev |> Array.head
        
        [|firstDigit; lastDigit|]
        |> String
        |> Int32.Parse
        
    let valueForInput2 (s:string) =
        s
        |> splitToTrimmedLines
        |> Seq.sumBy (fun line ->
                line |> valueForLine
            )
