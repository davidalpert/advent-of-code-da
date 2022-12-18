namespace AdventOfCode

open System.Reflection.Metadata

module RegolithReservoir =

    open System
    open System.Collections.Generic
    open FParsec
    open FParsec.Pipes
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
    
    let fillIn (shapes:Shape[]) : Point[] =
        shapes |> Array.map (fun shape ->
                shape |> Seq.pairwise
                |> Seq.map pointsConnecting
                |> Seq.concat
            )
        |> Seq.concat |> Seq.distinct |> Array.ofSeq
        
    type Scan = {
        rock : Point[]
        sandAppearsAt : Point
        sand : Point list
    }
    with
        member this.maxY = y (this.rock |> maxTuple)
        
        static member fromShapes (shapes:Shape seq) =
            {
                rock = shapes |> Array.ofSeq |> fillIn
                sandAppearsAt = (500,0)
                sand = []
            }
    
    module parser =
        let pPoint : Parser<Point,unit> =
            %% +.pint32 ?- %',' -- +.pint32 -%> auto
            
        let pShape : Parser<Shape,unit> =
            %% +.(pPoint * (qty[2..] / " -> ")) -- ws
            -%> Array.ofSeq
            
        let pScan =
            %% +.(pShape * qty[1..]) -|> Scan.fromShapes
            
        let parseScan = mustParse pScan

    let directlyBelow p = (fst p, snd p + 1)
    let diagonallyDownAndLeft p = (fst p - 1, snd p + 1)
    let diagonallyDownAndRight p = (fst p + 1, snd p + 1)
    
    let contains item collection = collection |> Seq.contains item
    let doesNotContain item collection = (collection |> Seq.contains item) |> not
    
    let sandMightFalTo p = [|directlyBelow p; diagonallyDownAndLeft p; diagonallyDownAndRight p|]
    
    let sandFallsNextFrom (from:Point) (scan:Scan) =
        sandMightFalTo from
        |> Array.tryFind (fun newSpot ->
            (scan.rock |> doesNotContain newSpot) && (scan.sand |> doesNotContain newSpot)
        )
    
    type SandStrategy = {
        fallingPath : Scan -> Point -> (Point * Point) option
        stopPouringWhen : Scan -> Point -> bool
    }
    
    let part1SandStrategy = {
        fallingPath =
            fun scan prev ->
                if (y prev) > scan.maxY then
                    None // we've fallen below the lowest rock
                else
                    match scan |> sandFallsNextFrom prev with
                    | Some next -> Some (next,next)
                    | None -> None // we can't any open space
                   
        stopPouringWhen = fun scan p -> y p > scan.maxY
    }
    
    let pathOfSandFrom strategy p (scan:Scan) =
        Array.concat [|
            Array.singleton p; // include p in the list; allows our sequence to end after the sand falls below the rock
            Array.unfold (strategy.fallingPath scan) p
        |]
        
    let pourSandUntilItStops strategy (scan:Scan) =
        Seq.unfold (fun s ->
            let grainFallsTo =
                s |> pathOfSandFrom strategy s.sandAppearsAt |> Seq.last
            
            match grainFallsTo with
            | p when strategy.stopPouringWhen scan p -> None
            | p ->
                let nextScan = { s with sand = p :: s.sand }
                Some(nextScan,nextScan)
        ) scan
        
    let part1_how_many_units_of_sand_come_to_rest_before_sand_starts_flowing_into_the_abyss_below (input:string) =
        let finalScan =
            input
            |> parser.parseScan
            |> pourSandUntilItStops part1SandStrategy
            |> Seq.last
                   
        finalScan.sand.Length
