namespace AdventOfCode

module day03_Mull_It_Over =

    open System
    open System.Collections.Generic
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    type Instruction =
        Mul of int * int

    module parser =
        let ws = spaces
        let ch = pchar

        let pMul =
            %% "mul(" -- +.pint32 -- "," -- +.pint32 -? ")"
            -|> fun x y -> Some(Mul(x, y))

        let pCorruptedChar =
            %% +.(%p<char>)
            -|> fun _ ->
                // return a None value of type Instruction options
                None

        let pSegment = %[pMul;pCorruptedChar]
        
        let pInstructionSet =
            %% +.(pSegment * qty.[0..])
            -|> fun possibleInstructions ->
                possibleInstructions
                |> Seq.choose id

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse pInstructionSet (input.Trim())

    module part2Parser =

        type ParserState =
        | MulEnabled
        | MulDisabled

        let ws = spaces
        let ch = pchar

        let pMul =
            %% +.getUserState -- "mul(" -- +.pint32 -- "," -- +.pint32 -? ")"
            -|> (fun s x y ->
                match s with
                | MulEnabled -> Some(Mul(x, y))
                | MulDisabled -> None
            )
            <!> "mul"

        let pDo = %% %"do()" ?- (setUserState MulEnabled) -|> None <!> "do"
        let pDont = %% %"don't()" ?- (setUserState MulDisabled) -|> None <!> "dont"

        let pCorruptedChar =
            %% +.(%p<char>)
            -|> fun _ -> None

        let pSegment = %[pDont;pDo;pMul;pCorruptedChar]

        let pInstructionSet =
            %% +.(pSegment * qty.[0..])
            -|> fun possibleInstructions ->
                possibleInstructions
                |> Seq.choose id

        let mustParse p (input:string) =
            match runParserOnString p MulEnabled "input" input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse pInstructionSet (input.Trim())

    let addUpResults (instructions: Instruction seq) =
        instructions
        |> Seq.map (fun instruction ->
            match instruction with
            | Mul(x, y) -> x * y)
        |> Seq.sum
