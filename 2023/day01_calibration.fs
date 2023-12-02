namespace AdventOfCode

module Calibration =
    open System
    open Input
    open FParsec
    open FParsec.Pipes

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

    module parser2 =
        let ws = spaces
        
        let pLiteralNumber (word:string) c =
            let wordChars = word.ToCharArray()
            let firstChar = wordChars |> Array.head
            let rest = wordChars |> Array.tail |> String
            let pMatchOnFirstCharOnlyIfRestMatches =
                // consume firstChar; only if rest matches (without consuming)
                pchar firstChar .>>? followedByString rest
            
            %% ws -? pMatchOnFirstCharOnlyIfRestMatches -|> c // and return c
            
        let pCalibrationChar : Parser<char,unit> =
            choice [
                pLiteralNumber "one" '1';
                pLiteralNumber "two" '2';
                pLiteralNumber "three" '3';
                pLiteralNumber "four" '4';
                pLiteralNumber "five" '5';
                pLiteralNumber "six" '6';
                pLiteralNumber "seven" '7';
                pLiteralNumber "eight" '8';
                pLiteralNumber "nine" '9';
                anyChar;
            ]
            
        let pCalibrationLine =
            %% +.(pCalibrationChar * qty.[1..])
            -|> fun cc -> cc |> Array.ofSeq |> String

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg
            
        let parseCalibrationInput (input:string) =
            mustParse pCalibrationLine input
    
    let preProcessLine2 (s:string) =
        s |> parser2.parseCalibrationInput
    
    let valueForLine2 (s:string) =
        s
        |> preProcessLine2
        |> valueForLine
        
    let valueForInput2 (s:string) =
        s
        |> splitToTrimmedLines
        |> Seq.sumBy (fun line ->
                line |> valueForLine2
            )
