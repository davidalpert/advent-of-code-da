namespace AdventOfCode

module day07_Camel_Cards =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    // The relative strength of each card follows this order,
    // where A is the highest and 2 is the lowest.
    type CardValue =
    | Ace   = 15
    | King  = 13
    | Queen = 12
    | Jack  = 11
    | Ten   = 10
    | Nine  = 9
    | Eight = 8
    | Seven = 7
    | Six   = 6
    | Five  = 5
    | Four  = 4
    | Three = 3
    | Two   = 2
    | Unrecognized = 0
   
    let cardValueFromChar c =
        match c with
        | 'A' -> CardValue.Ace
        | 'K' -> CardValue.King
        | 'Q' -> CardValue.Queen
        | 'J' -> CardValue.Jack
        | 'T' -> CardValue.Ten
        | '9' -> CardValue.Nine
        | '8' -> CardValue.Eight
        | '7' -> CardValue.Seven
        | '6' -> CardValue.Six
        | '5' -> CardValue.Five
        | '4' -> CardValue.Four
        | '3' -> CardValue.Three
        | '2' -> CardValue.Two
        | _   -> CardValue.Unrecognized

    // convenience function for printing
    let cardValueToChar c =
        match c with
        | CardValue.Ace   -> 'A'
        | CardValue.King  -> 'K'
        | CardValue.Queen -> 'Q'
        | CardValue.Jack  -> 'J'
        | CardValue.Ten   -> 'T'
        | CardValue.Nine  -> '9'
        | CardValue.Eight -> '8'
        | CardValue.Seven -> '7'
        | CardValue.Six   -> '6'
        | CardValue.Five  -> '5'
        | CardValue.Four  -> '4'
        | CardValue.Three -> '3'
        | CardValue.Two   -> '2'
        | _   -> '*'

    // Every hand is exactly one type.
    // From strongest to weakest, they are:
    type HandType =
    | FiveOfAKind  = 6
    | FourOfAKind  = 5
    | FullHouse    = 4
    | ThreeOfAKind = 3
    | TwoPair      = 2
    | OnePair      = 1
    | HighCard     = 0
 
    // A hand consists of five cards labeled one
    // of A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2.
    // Every hand is exactly one type.
    [<CustomEquality;CustomComparison>]
    type Hand = {
        cards : CardValue array
        Type : HandType
        bid  : int
    }
    with
        member this.str = sprintf "%s %A %d" (this.cards |> Array.map cardValueToChar |> String) this.Type this.bid

        static member handType (cc:CardValue array) =
            let cardsByType = cc |> Array.groupBy id |> Array.map (fun (c,cc) -> (c,cc.Length))
            // printfn "handType: %A - %A" cc cardsByType
            
            match cardsByType.Length with
            | 1 -> HandType.FiveOfAKind
            | 2 -> match (snd cardsByType[0]),(snd cardsByType[1]) with
                   | (1,4) -> HandType.FourOfAKind
                   | (4,1) -> HandType.FourOfAKind
                   | (3,2) -> HandType.FullHouse
                   | (2,3) -> HandType.FullHouse
                   | (_,_) -> HandType.HighCard
            | 3 -> match (snd cardsByType[0]),(snd cardsByType[1]),(snd cardsByType[2]) with
                   | (3,1,1) -> HandType.ThreeOfAKind
                   | (1,3,1) -> HandType.ThreeOfAKind
                   | (1,1,3) -> HandType.ThreeOfAKind
                   | (2,2,1) -> HandType.TwoPair
                   | (2,1,2) -> HandType.TwoPair
                   | (1,2,2) -> HandType.TwoPair
                   | (_,_,_) -> HandType.HighCard
            | 4 -> match (snd cardsByType[0]),(snd cardsByType[1]),(snd cardsByType[2]),(snd cardsByType[3]) with
                   | (2,1,1,1) -> HandType.OnePair
                   | (1,2,1,1) -> HandType.OnePair
                   | (1,1,2,1) -> HandType.OnePair
                   | (1,1,1,2) -> HandType.OnePair
                   | (_,_,_,_) -> HandType.HighCard
            | _ -> HandType.HighCard

        // needed to implement custom comparers
        override this.Equals other =
            match other with
            // compare only on cards and bid
            | :? Hand as h -> this.cards = h.cards && this.bid = h.bid
            | _ -> false
            
        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? Hand as h -> (this :> IComparable<_>).CompareTo h
                | _ -> -1
                
        interface IComparable<Hand> with
            member this.CompareTo other =
                match this.Type.CompareTo(other.Type) with
                | 0 ->
                    (*
                    If two hands have the same type, a second ordering rule takes effect.
                    Start by comparing the first card in each hand. If these cards are
                    different, the hand with the stronger first card is considered stronger.
            
                    If the first card in each hand have the same label, however,
                    then move on to considering the second card in each hand.
                    If they differ, the hand with the higher second card wins;
                    otherwise, continue with the third card in each hand, then
                    the fourth, then the fifth.
                    *)
                    let tieBreaker =
                       Seq.zip this.cards other.cards
                       |> Seq.map (fun (a,b) -> a.CompareTo(b))
                       |> Seq.tryFind (fun c -> c <> 0)
                    match tieBreaker with
                    | Some(c) ->
                        c
                    | None -> 0
                | x ->
                    x
                
        member this.isStrongerThan (other:Hand) =
            ((this :> IComparable<_>).CompareTo other) > 0
            
        static member build (c:string) b =
            let cards = c.ToCharArray() |> Array.map cardValueFromChar
            {
                cards = cards
                Type = Hand.handType cards
                bid = b
            }

    let rank (set:Hand array) =
        set |> Array.sort
        
    let totalWinnings (set:Hand array) =
        rank set
        |> Seq.mapi (fun i h -> h.bid * (i+1))
        |> Seq.sum

    module parser =
        let ws = spaces
        let ch = pchar
        
        let pHand =
            %% ws -? +.(anyString 5) -- ws -- +.pint32
            -|> Hand.build
        
        let pListOfHands =
            %% ws -- +.(pHand * qty[1..])
            -|> fun cc -> cc |> Array.ofSeq

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse pListOfHands input

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Array.ofSeq
