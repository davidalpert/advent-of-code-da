namespace AdventOfCode

open System.Diagnostics
open System.Security

module day10_Pipe_Maze =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    let surroundingPositions (x,y) =
        [
            x-1,y-1; x,y-1; x+1,y-1;
            x-1,y  ;        x+1,y  ;
            x-1,y+1; x,y+1; x+1,y+1;
        ]
    
    let north y = y - 1 // towards the top of the map
    let south y = y + 1 // towards the bottom of the map
    let east  x = x + 1 // to the right
    let west  x = x - 1 // to the left

    type Pos = {
        x: int
        y: int
    }
    with
        override this.ToString() = $"(%d{this.x},%d{this.y})"
        static member fromTuple (p:int*int) = { x = fst p; y = snd p }

        member this.north = { this with y = north this.y }
        member this.south = { this with y = south this.y }
        member this.east  = { this with x = east  this.x }
        member this.west  = { this with x = west  this.x }
        
        member this.surroundingPos =
            surroundingPositions (this.x, this.y)
            |> List.map Pos.fromTuple

    type Tile =
    | VerticalPipe     // | is a vertical pipe connecting north and south.
    | HorizontalPipe   // - is a horizontal pipe connecting east and west.
    | NorthEastBend    // L is a 90-degree bend connecting north and east.
    | NorthWestBend    // J is a 90-degree bend connecting north and west.
    | SouthWestBend    // 7 is a 90-degree bend connecting south and west.
    | SouthEastBend    // F is a 90-degree bend connecting south and east.
    | Ground           // . is ground; there is no pipe in this tile.
    | StartingPosition // S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
    with
        member x.ToChar() =
            match x with
            | VerticalPipe     -> '|'
            | HorizontalPipe   -> '-'
            | NorthEastBend    -> 'L'
            | NorthWestBend    -> 'J'
            | SouthWestBend    -> '7'
            | SouthEastBend    -> 'F'
            | Ground           -> '.'
            | StartingPosition -> 'S'

    let possibleConnectionPoints (p:Pos) (t:Tile) =
        (match t with
        | VerticalPipe     -> [p.north; p.south]
        | HorizontalPipe   -> [p.east;  p.west]
        | NorthEastBend    -> [p.north; p.east]
        | NorthWestBend    -> [p.north; p.west]
        | SouthWestBend    -> [p.south; p.west]
        | SouthEastBend    -> [p.south; p.east]
        | Ground           -> []
        | StartingPosition -> [p.north; p.south; p.east; p.west]
        ) |> Set

    exception InvalidStartingPosition of Pos * string
    exception AmbiguousNextStep of (Pos * Pos list)
    exception AmbiguousNextSteps of (Pos * Pos list) * (Pos * Pos list)
 
    type Map = {
        tiles : Tile array2d
        startingPosition : Pos
    }
    with
        member this.Item
            with get (p:Pos) = this.tiles[p.y,p.x]
        
        member this.contains (p:Pos) =
            this.tiles.GetLowerBound 0 <= p.y && p.y <= this.tiles.GetUpperBound 0 &&
            this.tiles.GetLowerBound 1 <= p.x && p.x <= this.tiles.GetUpperBound 1 

        member this.possibleConnectionPoints (p:Pos) =
            possibleConnectionPoints p this[p] |> Set.filter this.contains

        member this.areConnected (a:Pos) (b:Pos) =
            this.possibleConnectionPoints a |> Set.contains b &&
            this.possibleConnectionPoints b |> Set.contains a

        member this.positionsConnectedTo (p:Pos) =
            this.possibleConnectionPoints p
            |> Seq.filter (this.areConnected p)

        member this.positionsConnectedToExcluding (p:Pos) (x:Pos) =
            this.possibleConnectionPoints p |> Set.remove x
            |> Seq.filter (this.areConnected p)
            |> List.ofSeq

        member this.walkHalfway =
            let step0 = (this.startingPosition, this.startingPosition)
            
            let initialConnections = this.positionsConnectedTo this.startingPosition |> Array.ofSeq
            if initialConnections.Length <> 2 then
                raise (InvalidStartingPosition(this.startingPosition,$"found wrong number of initial connections %A{initialConnections}"))
            
            let step1 = (initialConnections[0],initialConnections[1])
            
            Seq.concat [
                [step0] |> Seq.ofList
                Seq.unfold (fun prev ->
                    let (step'', step') = prev
                    let (a'',b'') = step''
                    let (a',b') = step'
                    if a'' <> this.startingPosition && a'' = b'' then
                        None // the last one matched again, closing the loop; nothing left to do
                    else
                        // find next step of both
                        let connections = (this.positionsConnectedToExcluding a' a'', this.positionsConnectedToExcluding b' b'')
                        match connections with
                        | [nextA], [nextB] -> Some(prev, ((a',b'),(nextA, nextB)))
                        | [nextA],  nextB  -> raise (AmbiguousNextStep(b', nextB))
                        |  nextA , [nextB] -> raise (AmbiguousNextStep(a', nextA))
                        |  nextA ,  nextB  -> raise (AmbiguousNextSteps((a',nextA), (b',nextB)))
                ) (step0,step1)
                |> Seq.map snd
            ]
        
        member this.numberOfStepsToFarthestPoint =
            (this.walkHalfway |> Seq.length) - 1 // the walk is 0-indexed

    module parser =
        exception InvalidCharacterException of string
        
        let char2tile c =
            match c with
            | '|' -> VerticalPipe     // | is a vertical pipe connecting north and south.
            | '-' -> HorizontalPipe   // - is a horizontal pipe connecting east and west.
            | 'L' -> NorthEastBend    // is a 90-degree bend connecting north and east.
            | 'J' -> NorthWestBend    // a 90-degree bend connecting north and west.
            | '7' -> SouthWestBend    // is a 90-degree bend connecting south and west.
            | 'F' -> SouthEastBend    // a 90-degree bend connecting south and east.
            | '.' -> Ground           // is ground; there is no pipe in this tile.
            | 'S' -> StartingPosition // is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
            | c   -> raise (InvalidCharacterException $"%c{c}")
        
        // let valuesWithPositions (haystack:_ array2d) =
        //     seq {
        //         for y in 1 .. (Array2D.length1 haystack) do
        //             for x in 1 .. (Array2D.length2 haystack - 1) do
        //                 yield (haystack[y,x],Pos.fromTuple(x,y))
        //     }

        let parseInput (input:string) =
            let tiles = input |> splitTo2DArray char2tile
            let startingPos = tiles |> find2D StartingPosition
            {
                tiles = tiles
                startingPosition = Pos.fromTuple(startingPos.Value) // our problem statement ensures we have a starting position
            }

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Array.ofSeq
