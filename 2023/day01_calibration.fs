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
        