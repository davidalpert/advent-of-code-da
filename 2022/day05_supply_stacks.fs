namespace AdventOfCode

module SupplyStacks =

    open System
    open FSharp.Stats
    
    // helpers
    
    let crateID c = int c - int 'A' + 1
    let crateName i = char (i - 1 + int 'A')
    
    let firstPairFromArray ss =
        match Array.length ss with
        | l when l >= 2 -> (ss[0], ss[1])
        | _ -> failwithf $"firstPairFromArray requires at least 2 elements; found %d{ss |> Array.length}"
        
    type CraneInstruction =
    | CraneInstruction of int * int * int
        
    module parser =
        open FParsec
        open FParsec.Pipes
        
        let ch = pchar
        let ws = spaces
        
        // atoms
        // type Crate = Crate of char
        
        // organisms
        // type StacksOfCrates = Map<int, char list> (or simply a char list[])
        
        let pCrate =
            %% %'[' ?- +.p<char> -- %']'
            -|> (fun c -> c |> Some)
            
        let pEmptySpace =
            %% %"   " -%> None
            
        let pCrateOrNot =
            %[pCrate; pEmptySpace]
            
        let pRowOfCratesInSpace =
            %% +.(pCrateOrNot * (qty[1..] / ' ')) -- (restOfLine true) -%> auto
            
        let pStackKey =
            %% %' ' -- +.p<char> -- %' ' -%> auto
            
        let pRowOfKeys =
            %% +.(pStackKey * (qty[1..] / ' ')) -- ws -%> auto
            
        // row 0: None,   Some D, None
        // row 1: Some N, Some C, None
        // row 2: Some Z, Some M, Some P
        //  keys: 1       2       3
        // index: 0       1       2
        
        let pInitialState =
            %% +.(pRowOfCratesInSpace * qty[1..]) -- +.pRowOfKeys
            -%> fun rows keyRow ->
                // ResizeArray -> Array
                let crateRows = rows |> Seq.map (fun r -> r |> Array.ofSeq) |> Array.ofSeq
                
                keyRow
                |> Array.ofSeq
                |> Array.mapi (
                      fun i _ ->
                          // printfn $"collecting crates for stack '%d{i+1}'"
                          let x =
                              crateRows
                              |> Array.map (fun r -> r[i])
                              |> List.ofSeq
                          // printfn $"found: %A{x}"
                          x
                   )
                |> Array.map (
                        fun crates ->
                            crates |> List.choose id
                    )
            
        let pInstruction =
            %% %"move" -- ws -- +.p<int> -- ws -- %"from" -- ws -- +.p<int> -- ws -- %"to" -- ws -- +.p<int> -- ws
            -|> fun n fromStackNumber toStackNumber -> CraneInstruction(n, fromStackNumber - 1, toStackNumber - 1)
            // stack numbers are 1-indexed; when collected into an array they map to 0-indexed
            // we'll translate numbers to index here to avoid off-by-one errors later
            
        let pInstructionSet = pInstruction * qty[1..]
        
        // parseFns
        
        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg
            
        let parseState (input:string) =
            mustParse pInitialState input
            
        let parseInstructions (input:string) =
            mustParse pInstructionSet input

    let topCrates (cc: char list[]) =
        cc
        |> Array.map (fun crates ->
                match crates with
                | top::rest -> Some(top)
                | [] -> None
            )
        
    // let join (delim : string) (items : seq<'items>) =
    //     String.Join(delim, items)
    //     
    // let formatStacks (cc : char list[]) =
    //     cc
    //     |> Array.mapi (fun i crates ->
    //            $"%d{i}: %s{crates |> Array.ofList |> String}"
    //         )
    //     |> join "\n"
    
    let toInitialStateAndInstructions (s: string) =
        let initialState, instructions = s.Trim("\r\n".ToCharArray()).Split("\n\n") |> firstPairFromArray
        
        let parsedState = parser.parseState initialState
        let parsedInstructions = parser.parseInstructions instructions
        
        parsedState, parsedInstructions
        
    let part1_which_crates_end_up_on_top_of_each_stack (s:string) =
        let stacks, instructions = s |> toInitialStateAndInstructions
        
        // CrateMover 9000
        instructions
        |> Array.ofSeq
        |> Array.iter (fun (CraneInstruction(n, fromIndex, toIndex)) ->
                seq { 1..n }
                |> Seq.iter (fun _ ->
                    match stacks[fromIndex] with
                    | head::tail ->
                        stacks[toIndex] <- head::stacks[toIndex]
                        stacks[fromIndex] <- tail
                    | [] -> ()
                )
            )
        
        stacks
        |> topCrates
        |> Array.choose id
        |> String
        
    let part2_which_crates_end_up_on_top_of_each_stack (s:string) =
        let stacks, instructions = s |> toInitialStateAndInstructions
        
        // CrateMover 9001
        instructions
        |> Array.ofSeq
        |> Array.iter (fun (CraneInstruction(n, fromIndex, toIndex)) ->
                let lifted, remaining = stacks[fromIndex] |> List.splitAt n
                
                stacks[toIndex] <- List.concat [lifted; stacks[toIndex]]
                stacks[fromIndex] <- remaining
            )
        
        stacks
        |> topCrates
        |> Array.choose id
        |> String
        
