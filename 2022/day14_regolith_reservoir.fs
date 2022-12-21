namespace AdventOfCode

module RegolithReservoir =

    open System
    open System.Collections.Generic
    open FParsec
    open FParsec.Pipes
    open FSharpAux
    open utils

    type Point = int * int // x * y lets us use fst to access x and snd to access y
    let x p = fst p
    let y p = snd p
    
    type Shape = Point[]
    
    let pointsConnecting (p1,p2) =
        let min = [p1;p2] |> minTuple
        let max = [p1;p2] |> maxTuple
        seq { x min .. x max } |> Seq.map (fun x ->
            seq { y min .. y max } |> Seq.map (fun y ->
                x,y
            )
        )
        |> Seq.concat |> Array.ofSeq
    
    let fillIn (shapes:Shape seq) : Point[] =
        shapes |> Seq.map (fun shape ->
                shape |> Seq.pairwise
                |> Seq.map pointsConnecting
                |> Seq.concat
            )
        |> Seq.concat |> Seq.distinct |> Array.ofSeq
        
    let sandPoursInFrom = (500,0)
    
    type ScannerStrategy = {
        maxDepth: Point seq -> int
        stopPouring: Point seq -> Point -> bool
    }
    
    let part1ScannerStrategy = {
        maxDepth = fun rocks -> rocks |> Seq.maxBy y |> y
        stopPouring = fun rocks p -> y p < (rocks |> Seq.maxBy y |> y)
    }
    
    let part2ScannerStrategy = {
        maxDepth = fun rocks -> (rocks |> Seq.maxBy y |> y) + 2
        stopPouring = fun _ p -> p = sandPoursInFrom
    }
    
    type Scan = {
        grid: char[,]
        maxDepth: int
        xOffset: int
        stopWhenPreviousGrainSettledAt: Point -> bool
    }
    with
        member this.xo p = (x p) - this.xOffset
        
        static member fromRocks (strategy:ScannerStrategy) rocks =
            let maxDepth = rocks |> strategy.maxDepth
            let lowerLimit = maxDepth + 1
            // printfn $"lowerLimit: %d{lowerLimit}"
            let maxWidth = (lowerLimit * 2) + 1
            // printfn $"maxWidth: %d{maxWidth}"
            let xOffset = (x sandPoursInFrom) - (maxWidth / 2)
            // printfn $"xOffset: %d{xOffset}"
            let xo p = (x p) - xOffset
            // let ox n = n + xOffset
            
            let grid = Array2D.create maxWidth lowerLimit '.'
            rocks |> Seq.iter (fun (p:Point) ->
                // printfn $"%d{x p},%d{y p}"
                grid[xo p, y p] <- '#')
            grid[xo sandPoursInFrom, y sandPoursInFrom] <- '+'
            
            {
                grid = grid
                maxDepth = maxDepth
                xOffset = xOffset
                stopWhenPreviousGrainSettledAt = (fun p -> y p > maxDepth)
            }
    
    let ox scan n = n + scan.xOffset
    let xo scan n = n - scan.xOffset
    
    module parser =
        let pPoint : Parser<Point,unit> =
            %% +.pint32 ?- %',' -- +.pint32 -%> auto
            
        let pShape : Parser<Shape,unit> =
            %% +.(pPoint * (qty[2..] / " -> ")) -- ws
            -%> Array.ofSeq
            
        let pAllRocks =
            %% +.(pShape * qty[1..]) -|> fillIn
            
        let parseScannerInput = mustParse pAllRocks

    let directlyBelow p = (fst p, snd p + 1)
    let diagonallyDownAndLeft p = (fst p - 1, snd p + 1)
    let diagonallyDownAndRight p = (fst p + 1, snd p + 1)
    
    let contains item collection = collection |> Seq.contains item
    let doesNotContain item collection = (collection |> Seq.contains item) |> not
    
    let sandMayFallTo p = [|directlyBelow p; diagonallyDownAndLeft p; diagonallyDownAndRight p|]
    
    let sandFallsNextAfter (from:Point) (scan:Scan) =
        // (y from) < scan.maxDepth
        sandMayFallTo from
        |> Array.tryFind (fun newSpot ->
            // if (y newSpot > scan.maxDepth) then
            //     printfn $"%A{newSpot} %A{scan.maxDepth}"
                
            scan.stopWhenPreviousGrainSettledAt newSpot || scan.grid[scan.xo newSpot, y newSpot] = '.'
        )
    
    let pathOfSandFrom p (scan:Scan) =
        Array.concat [|
            Array.singleton p; // include p in the list; allows our sequence to end after the sand falls below the rock
            Array.unfold (fun (prev) ->
                // prev |> log (fun p -> sprintf $"%A{p}") |> ignore
                if scan.stopWhenPreviousGrainSettledAt prev then
                    None // done pouring
                else
                  match scan |> sandFallsNextAfter prev with
                  | Some next -> Some (next,next)
                  | None -> None // we can't any open space
            ) p
        |]
        
    let pourSandUntilItFallsBelowTheRock (scan:Scan) =
        Seq.unfold (fun s ->
            let grainFallsTo =
                s |> pathOfSandFrom sandPoursInFrom |> Seq.last
            
            match grainFallsTo with
            | p when scan.stopWhenPreviousGrainSettledAt p -> None
            | p ->
                s.grid[s.xo p, y p] <- 'o'
                Some(s,s)
        ) scan
        
    let countGrainsOfSand (scan:Scan) =
        scan.grid
        |> Array2D.countDistinctBy id
        |> Array.find (fun c -> (fst c) = 'o')
        |> snd
        
    let part1_how_many_units_of_sand_come_to_rest_before_sand_starts_flowing_into_the_abyss_below (input:string) =
        let finalScan =
            input
            |> parser.parseScannerInput
            |> Scan.fromRocks part1ScannerStrategy
            |> pourSandUntilItFallsBelowTheRock
            |> Seq.last
                   
        finalScan |> countGrainsOfSand
        
        