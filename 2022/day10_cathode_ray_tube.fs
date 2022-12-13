namespace AdventOfCode

module CathodeRayTube =

    open FParsec
    open FParsec.Pipes

    type Instruction =
    | Add of int
    | Noop
    
    module parser =
        let ws = spaces
        
        let pAdd = %% ws -- %"addx" ?- ws -- +.pint32 -- ws -|> Add
        let pNoop = %% ws -- %"noop" ?- ws -%> Noop
        
        let pInstruction = %[pAdd; pNoop]
        let pInstructions = many1 pInstruction
            
        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg
            
        let parseInstructions (input:string) =
            mustParse pInstructions input
        
    type Cycle = {
        valueDuring : int
        number : int
    }
    
    let signalStrength c = c.number * c.valueDuring 
    
    type RegisterStateWithInstructions = {
        lastCycle: Cycle
        remaining: Instruction list
    }
    
    let initialState instructions =
        {
             lastCycle = {
                valueDuring = 0;
                number = 0;
             }
             remaining = instructions
        }
        
    let apply (instructions: Instruction list) =
        let paddedInstructions =
            instructions
            |> List.collect (fun instruction ->
                    match instruction with
                    | Noop -> [Noop]
                    | Add(i) -> [Noop; Add(i)] // simulate that the op takes two cycles by padding it with Noop
                )
        // printfn $"\npaddedInstructions: %A{paddedInstructions}\n"
        
        Seq.unfold (fun state ->
                match state.lastCycle with
                | c when c.number = 0 ->
                    let nextCycle = { number = 1; valueDuring = 1 }
                    Some(nextCycle, {state with lastCycle = nextCycle})
                | _ -> 
                    let nextCycle = { state.lastCycle with number = state.lastCycle.number + 1 }
                    
                    match state.remaining with
                    | instruction :: rest ->
                        // printfn $"completed cycle %d{nextCycle.number}; applied %A{instruction}"
                        match instruction with
                        | Add(i) ->
                            let nextCycleWithValue = { nextCycle with valueDuring = nextCycle.valueDuring + i }
                            Some(
                                nextCycleWithValue,
                                { state with lastCycle = nextCycleWithValue; remaining = rest }
                            )
                        | Noop ->
                            Some(nextCycle, {state with lastCycle = nextCycle; remaining = rest})
                    | [] ->
                        // printfn "no more instructions"
                        Some(nextCycle, {state with lastCycle = nextCycle})
                
            ) (paddedInstructions |> initialState)
        
    let part1_whatIsTheSumOfNSampledSignalStrengths n (input: string) =
        input
        |> parser.parseInstructions
        |> apply
        |> Seq.skip 19 // so that next cycle is number 20
        |> Seq.chunkBySize 40
        |> Seq.take n
        |> Array.ofSeq
        |> Array.map (fun cc -> cc[0])
        |> Array.sumBy signalStrength
            
