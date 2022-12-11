namespace AdventOfCode

module RopeBridge =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    type Coordinate = { x: int; y: int }
    
    type Simulation = {
        startingPosition: Coordinate
        visited: Coordinate list
        head: Coordinate
        tail: Coordinate
    }
    with
        static member initialState = {
            head = { x = 0; y = 0 }
            tail = { x = 0; y = 0 }
            startingPosition = { x = 0; y = 0 }
            visited = [];
        }
        
        member this.allCoordinates =
            Seq.concat [this.visited; [this.startingPosition; this.head; this.tail]]
            
        member this.farthestCoordinates =
            let maxX = this.allCoordinates |> Seq.maxBy (fun c -> c.x)
            let maxY = this.allCoordinates |> Seq.maxBy (fun c -> c.y)
            (maxX, maxY)
            
    let simToString (sim:Simulation) =
        let xMax,yMax = sim.farthestCoordinates
            
        let wasVisited x y =
           sim.allCoordinates |> Seq.contains { x = x; y = y }
                
        let parts =
            seq { yMax.y .. -1 .. 0 }
            |> Seq.map (fun y ->
                seq { 0 .. xMax.x }
                |> Seq.map (fun x ->
                    match { x = x; y = y } with
                    | c when c = sim.head -> 'H'
                    | c when c = sim.tail -> 'T'
                    | c when c = sim.startingPosition -> 's'
                    | _ when wasVisited x y -> '#'
                    | _ -> '.'
                )
                |> Array.ofSeq
                |> String
            )
            |> Array.ofSeq
        
        // sprintf $"head: %d{sim.head.x}, %d{sim.head.y}" + "\n" +
        // sprintf $"tail: %d{sim.tail.x}, %d{sim.tail.y}" + "\n" +
        String.Join("\n", parts)
        
    let directlyAbove b a = a.x = b.x && a.y > b.y
    let directlyBelow b a = a.x = b.x && a.y < b.y
    let directlyLeftOf b a = a.x < b.x && a.y = b.y
    let directlyRightOf b a = a.x > b.x && a.y = b.y
    
    let diagonallyAboveAndLeftOf b a = a.y > b.y && a.x < b.x
    let diagonallyAboveAndRightOf b a = a.y > b.y && a.x > b.x
    let diagonallyBelowAndLeftOf b a = a.y < b.y && a.x < b.x
    let diagonallyBelowAndRightOf b a = a.y < b.y && a.x > b.x
            
    let overlapping a b = a.x = b.x && a.y = b.y
    let touching a b =
        false
        || (a.x = b.x - 1 && a.y = b.y - 1) || (a.x = b.x && a.y = b.y - 1) || (a.x = b.x + 1 && a.y = b.y - 1)
        || (a.x = b.x - 1 && a.y = b.y    ) || (a.x = b.x && a.y = b.y    ) || (a.x = b.x + 1 && a.y = b.y    )
        || (a.x = b.x - 1 && a.y = b.y + 1) || (a.x = b.x && a.y = b.y + 1) || (a.x = b.x + 1 && a.y = b.y + 1)
        
    let notTouching a b = (touching a b) |> not
                 
    let absDifference a b = if a - b > 0 then a - b else b - a
    let absDistanceBetween a b =
         {
           x = absDifference a.x b.x
           y = absDifference a.y b.y
         }
                 
    let moveHead (direction: string) sim =
            let moveHead direction =
                match direction with
                | "U" -> { sim.head with y = sim.head.y + 1 }
                | "D" -> { sim.head with y = sim.head.y - 1 }
                | "R" -> { sim.head with x = sim.head.x + 1 }
                | "L" -> { sim.head with x = sim.head.x - 1 }
                | _ -> failwithf $"'%s{direction}' is not a recognized direction"
                
            let moveTail head tail =
                let d = absDistanceBetween head tail
                // if head.x = tail.x && head.y > tail.y && d.y > 1 then
                match (head,tail) with
                | (h,t) when h |> directlyAbove t && d.y > 1 -> { tail with y = tail.y + 1 }
                | (h,t) when h |> directlyBelow t && d.y > 1 -> { tail with y = tail.y - 1 }
                | (h,t) when h |> directlyLeftOf t && d.x > 1 -> { tail with x = tail.x - 1 }
                | (h,t) when h |> directlyRightOf t && d.x > 1 -> { tail with x = tail.x + 1 }
                | (h,t) when h |> diagonallyAboveAndRightOf t && h |> notTouching t -> { x = tail.x + 1; y = tail.y + 1 }
                | (h,t) when h |> diagonallyAboveAndLeftOf t && h |> notTouching t -> { x = tail.x - 1; y = tail.y + 1 }
                | (h,t) when h |> diagonallyBelowAndRightOf t && h |> notTouching t -> { x = tail.x + 1; y = tail.y - 1 }
                | (h,t) when h |> diagonallyBelowAndLeftOf t && h |> notTouching t -> { x = tail.x - 1; y = tail.y - 1 }
                | _ -> tail
                 
            let newHead = moveHead direction
            let newTail = moveTail newHead sim.tail
                
            {
                sim with
                    head = newHead
                    tail = newTail
                    visited = (newTail :: sim.visited)
            }
            
    let moveHeadNTimes (instruction: string * int) (sim:Simulation) =
        let direction, n = instruction
        
        let mutable s = sim
        seq { 1 .. n } |> Seq.iter (fun _ -> s <- s |> moveHead direction)
        s
        
    let toInstructions (input: string) =
        input.Trim()
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
              let parts = s.Split(" ")
              (parts[0], Int32.Parse(parts[1]))
            )
        |> Array.ofSeq
        
    let part1_howManyPositionsDidTheTailVisitOnce (input: string) =
        let endState =
            input
            |> toInstructions
            |> Array.fold (fun s i -> s |> moveHeadNTimes i) Simulation.initialState

        Seq.concat [endState.visited; [endState.tail]]
        |> Seq.distinct
        |> Seq.length