namespace AdventOfCode

module DistressSignal =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open utils
    open FParsec
    open FParsec.Pipes
    
    // Packet data consists of lists and integers.
    type PacketData =
    | PList of PacketData[]
    | PValue of int
    
    let toPList (values: PacketData seq) = values |> Array.ofSeq |> PList

    module parser =
        let ws = spaces
        
        // define a PacketDatum parser which forwards to mutable reference pPacketDatumRef
        let pPacketData, pPacketDataRef = createParserForwardedToRef<PacketData,unit>()
        
        let pValue =
            %% +.pint32 -|> PValue
            // <!> "pValue"
            
        let pList =
            %% %'[' ?- +.(%[pPacketData] * (qty[0..] / ',')) -- %']' -- ws -|> toPList
            // <!> "pList"

        pPacketDataRef.Value <- %[pList;pValue]
        
        // our outermost packets are always lists
        let pPacket = pList
        
        let pPairOfPackets =
            %% +.pPacket
            ?- +.pPacket
            -%> auto
            // <!> "pPairOfPackets"

        // Your list consists of pairs of packets; pairs are separated by a blank line.
        let pInput =
            %% +.(pPairOfPackets * qty[1..])
            -|> Array.ofSeq // array because the size of input is fixed so we'll allocate it all at once
            // <!> "pInput"
        
        let mustParse p (input:string) =
            match run p (input.Trim()) with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg
            
        let parse (input:string) = mustParse pInput input

    let isSome v =
        match v with | Some(_) -> true | None -> false
        
    let rec doCompare pair = 
        match pair with
        // If the left integer is lower than the right integer, the inputs are in the right order.
        | PValue(left),PValue(right) when left < right -> Some(true)
        // If the left integer is higher than the right integer, the inputs are not in the right order. 
        | PValue(left),PValue(right) when left > right -> Some(false)
        // Otherwise, the inputs are the same integer; continue checking the next part of the input.
        | PValue(left),PValue(right) when left = right -> None
        // If both values are lists,
        | PList(left),PList(right) ->
            // compare the first value of each list, then the second value, and so on.
            match Seq.zip left right |> Seq.map doCompare |> Seq.tryFind isSome with
            | Some(result) -> result
            | None ->
                // If the left list runs out of items first, the inputs are in the right order.
                if left.Length < right.Length then
                   Some(true)
                // If the right list runs out of items first, the inputs are not in the right order.
                else if right.Length < left.Length then
                   Some(false)
                // If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
                else None
        // If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison.
        | PValue(_),PList(_) -> doCompare([fst pair] |> toPList, snd pair)
        | PList(_),PValue(_) -> doCompare(fst pair, [snd pair] |> toPList)
        
    let isInCorrectOrder pair =
        match pair |> doCompare with
        | Some(result) -> result
        | None -> false
        
    let chooseWhenCorrect i pair =
        match pair |> isInCorrectOrder with
        | true -> Some(i+1)
        | false -> None
        
    let indicesOfPairsOfPocketsInTheCorrectOrder (input:string) =
        input
        |> parser.parse
        |> Array.mapi chooseWhenCorrect
        |> Array.choose id

    let rec part1_what_is_the_sum_of_indices_of_pairs_of_packets_which_are_already_in_the_right_order (input:string) =
        input
        |> indicesOfPairsOfPocketsInTheCorrectOrder
        |> Array.sum
        
