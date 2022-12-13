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
        tail: Coordinate list
    }
    with
        static member initialState numberOfKnots = {
            head = { x = 0; y = 0 }
            tail = seq { 1..(numberOfKnots-1) } |> Seq.map (fun _ -> { x = 0; y = 0 }) |> List.ofSeq
            startingPosition = { x = 0; y = 0 }
            visited = [];
        }
        
        member this.allCoordinates =
            Seq.concat [this.visited; [this.startingPosition; this.head]; this.tail]
            
        member this.farthestCoordinates =
            let maxX = this.allCoordinates |> Seq.maxBy (fun c -> c.x)
            let maxY = this.allCoordinates |> Seq.maxBy (fun c -> c.y)
            (maxX, maxY)
            
    let rec lastKnot tail =
        match tail with
        | [c] -> c
        | c :: rest -> lastKnot rest
        | _ -> failwith "need at least one item"

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
                    | c when sim.tail |> List.contains c ->
                        let i = sim.tail |> List.findIndex (fun x -> x = c)
                        match i with
                        | x when x = (sim.tail.Length - 1) -> 'T'
                        | _ -> char (i + int '0' + 1)
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
                let knots = head :: tail |> Array.ofList // create an array so we can update as we iterate
                   
                seq { 0 .. (knots.Length-1) }
                |> Array.ofSeq
                |> Array.pairwise
                |> Array.iteri (fun i (hIndex,tIndex) ->
                    let hh = knots[hIndex]
                    let tt = knots[tIndex]
                    let d = absDistanceBetween hh tt
                    // printfn $"knot %d{i} (%d{hh.x},%d{hh.y}) <-- knot %d{i+1} (%d{tt.x},%d{tt.y}) : distance is: %d{d.x},%d{d.y}"
                    knots[tIndex] <-
                        match (hh,tt) with
                        | (h,t) when h |> directlyAbove t && d.y > 1 -> { t with y = t.y + 1 }
                        | (h,t) when h |> directlyBelow t && d.y > 1 -> { t with y = t.y - 1 }
                        | (h,t) when h |> directlyLeftOf t && d.x > 1 -> { t with x = t.x - 1 }
                        | (h,t) when h |> directlyRightOf t && d.x > 1 -> { t with x = t.x + 1 }
                        | (h,t) when h |> diagonallyAboveAndRightOf t && h |> notTouching t -> { x = t.x + 1; y = t.y + 1 }
                        | (h,t) when h |> diagonallyAboveAndLeftOf t && h |> notTouching t -> { x = t.x - 1; y = t.y + 1 }
                        | (h,t) when h |> diagonallyBelowAndRightOf t && h |> notTouching t -> { x = t.x + 1; y = t.y - 1 }
                        | (h,t) when h |> diagonallyBelowAndLeftOf t && h |> notTouching t -> { x = t.x - 1; y = t.y - 1 }
                        | _ -> tt
                )
                
                knots
                |> Seq.skip 1 // the head was added temporarily to use in pairwise
                |> List.ofSeq
                 
            let newHead = moveHead direction
            let newTail = moveTail newHead sim.tail
                
            {
                sim with
                    head = newHead
                    tail = newTail
                    visited = ((lastKnot newTail) :: sim.visited)
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
        
    let howManyPositionsDidTheLastOfNKnotsVisitOnce n (input:string) =
        let endState =
            input
            |> toInstructions
            |> Array.fold (fun s i -> s |> moveHeadNTimes i) (Simulation.initialState n)

        Seq.concat [endState.visited; [lastKnot endState.tail]]
        |> Seq.distinct
        |> Seq.length
        
    let part1_howManyPositionsDidTheTailVisitOnce (input: string) =
        input |> howManyPositionsDidTheLastOfNKnotsVisitOnce 2
        
