namespace AdventOfCode

module day04_Scratchcards =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    type Card =
        {
            id: int
            winningNumbers: int array
            numbers: int array
        }
        static member build(id, winners, numbers) =
            { id = id; winningNumbers = winners; numbers = numbers; }
            
        member this.matches =
            this.numbers
            |> Array.where (fun n -> this.winningNumbers |> Array.contains n)

        member this.points =
            let registerMatch p =
                match p with
                | 0 -> 1     // first match is worth 1 point
                | p -> p * 2 // additional matches double the point value

            this.matches
            |> Array.fold (fun p _ -> registerMatch p) 0

    let sumOfPoints (cards:Card seq) =
        cards |> Seq.sumBy (fun c -> c.points)

    let countsByCardPart2 (cards:Card array) =
        let countsByCard = System.Linq.Enumerable.ToDictionary(cards |> Array.map (fun c -> c.id, 1), fst, snd)
        let maxCount = cards.Length
        
        let min (x:int) (y:int) = System.Math.Min(x,y)
        
        cards
        |> Seq.sortBy (fun c -> c.id)
        |> Seq.takeWhile (fun c -> c.id < maxCount)
        |> Seq.iter (fun c ->
               let copiesOfThisCard = countsByCard[c.id]
               let numberOfCardsWon = c.matches.Length
               let firstCardWon = c.id + 1
               let lastCardWon = c.id + numberOfCardsWon
               // printfn "-- card id: %d [%d matches; won %d .. %d]" c.id numberOfCardsWon firstCardWon lastCardWon
               seq { firstCardWon .. lastCardWon }
               |> Seq.iter (fun nid ->
                   if nid <= maxCount then
                       countsByCard[nid] <- countsByCard[nid] + copiesOfThisCard
               )
            )
        
        countsByCard
        
    let countOfTotalCards2 (cards:Card array) =
        cards
        |> countsByCardPart2
        |> Seq.sumBy (fun pair -> pair.Value)

    module parser =
        let ws = spaces
        let ch = pchar
        
        let pCard =
            %% ws -- %"Card" -- ws -- +.(pint32) -- %':' -- ws
            -- +.((pint32 .>> ws) * qty[1..])    -- %'|' -- ws
            -- +.((pint32 .>> ws) * qty[1..])
            -|> fun id w n -> Card.build(id, w.ToArray(), n.ToArray())

        let pStackOfCards =
            %% ws -- +.(pCard * qty[1..])
            -|> fun cc -> cc.ToArray()

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse pStackOfCards input

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Array.ofSeq
