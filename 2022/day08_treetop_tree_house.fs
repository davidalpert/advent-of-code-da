namespace AdventOfCode

module TreetopTreeHouse =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    type Coordinate = { x: int; y: int }
    type CoordinateWithHeight = { x: int; y: int; height: int }
    
    type GridOfTrees(data: int[][]) =
       let heights = data
       member this.width = heights.Length
       member this.height = match this.width with
                            | 0 -> 0
                            | _ -> heights.Length
                            
       member this.asCoordinates =
           heights |> Array.mapi (fun y xx ->
                    xx |> Array.mapi (fun x h ->
                        { x = x; y = y; }
                   )
               )
           |> Array.concat
           
       member this.heightOf (c: Coordinate) =
           heights[c.x][c.y]
           
       member this.visibleFromEdge (c: Coordinate) =
           let heightOfThisCoordinate = this.heightOf c
           // printfn $"height of (%d{c.x},%d{c.y}) is %d{heightOfThisCoordinate}"
           if c.x = 0 || c.x = (this.width - 1) || c.y = 0 || c.y = (this.height - 1) then
               // All of the trees around the edge of the grid are visible -
               // since they are already on the edge, there are no trees to
               // block the view.
               true
           else
               let coordsToTheLeft = seq { (c.x - 1) .. -1 .. 0 } |> Seq.map (fun x -> { x = x; y = c.y })
               let coordsToTheRight = seq { (c.x + 1) .. (this.width - 1) } |> Seq.map (fun x -> { x = x; y = c.y })
               let coordsToTheNorth = seq { (c.y - 1) .. -1 .. 0 } |> Seq.map (fun y -> { x = c.x; y = y })
               let coordsToTheSouth = seq { (c.y + 1) .. (this.width - 1) } |> Seq.map (fun y -> { x = c.x; y = y })
               
               let areAllShorter (cc: Coordinate seq) =
                  cc
                  |> Seq.map (fun t ->
                        let height = this.heightOf t
                        // printfn $"inspecting: (%d{t.x},%d{t.y}): %d{height}"
                        height
                      )
                  |> Seq.forall (fun t -> t < heightOfThisCoordinate)
                   
               (
                    (coordsToTheLeft  |> areAllShorter)
                 || (coordsToTheRight |> areAllShorter)
                 || (coordsToTheNorth |> areAllShorter)
                 || (coordsToTheSouth |> areAllShorter)
               )
       
       member this.allVisibleFromEdge =
           this.asCoordinates |> Seq.filter this.visibleFromEdge
           
       member this.scenicScoreForCoordinate (c: Coordinate) =
           let coordsToTheLeft = seq { (c.x - 1) .. -1 .. 0 } |> Seq.map (fun x -> { x = x; y = c.y })
           let coordsToTheRight = seq { (c.x + 1) .. (this.width - 1) } |> Seq.map (fun x -> { x = x; y = c.y })
           let coordsToTheNorth = seq { (c.y - 1) .. -1 .. 0 } |> Seq.map (fun y -> { x = c.x; y = y })
           let coordsToTheSouth = seq { (c.y + 1) .. (this.width - 1) } |> Seq.map (fun y -> { x = c.x; y = y })
           
           let heightOfThisCoordinate = this.heightOf c
           
           let maxViewingDistance (cc: Coordinate seq) =
               cc
               |> Seq.tryFindIndex (fun d -> (this.heightOf d) >= heightOfThisCoordinate)
               |> fun m -> match m with
                           | Some i -> i + 1
                           | None -> cc |> Seq.length
                           
           (coordsToTheLeft |> maxViewingDistance)
           * (coordsToTheRight |> maxViewingDistance)
           * (coordsToTheNorth |> maxViewingDistance)
           * (coordsToTheSouth |> maxViewingDistance)
           
    let toGrid (input: string) =
        input
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
                s.ToCharArray() |> Array.map (fun c -> int c - int '0')
            )
        |> Array.ofSeq
        |> GridOfTrees

    let part1_howManyTreesAreVisibleOutsideTheGrid (input: string) =
        let grid = input |> toGrid
        
        grid.allVisibleFromEdge |> Seq.length
        
    let part2_whatIsTheHighestScenicScorePossibleForAnyTree (input: string) =
        let grid = input |> toGrid
        
        grid.asCoordinates |> Array.map grid.scenicScoreForCoordinate |> Array.max