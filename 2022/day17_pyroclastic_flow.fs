namespace AdventOfCode

open AdventOfCode.RockPaperScissors

module PyroclasticFlow =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open utils
    open FParsec
    open FParsec.Pipes
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    let rockShapesTxt =
        """
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
"""
    type RockShape = | HLine | Plus | RightAngle | VLine | Square
    
    let shapeSequence = [|HLine; Plus; RightAngle; VLine; Square|]
    let shapeSource = repeatingSequenceOf shapeSequence
    
    type Position = {
        x: int64 // 0-indexed
        y: int64 // 1-indexed
    }
    
    let positionModsByShape s =
        match s with
        | HLine -> [ 0,0; 1,0; 2,0; 3,0 ]
        | Plus -> [
                       (1,2);
                (0,1); (1,1); (2,1);
                       (1,0)
            ]
        | RightAngle -> [
                              (2,2);
                              (2,1);
                (0,0); (1,0); (2,0)
            ]
        | VLine -> [
                (0,3)
                (0,2)
                (0,1)
                (0,0)
            ]
        | Square -> [
                (0,1); (1,1);
                (0,0); (1,0);
            ]
    
    // The tall, vertical chamber is exactly seven units wide.
    let stoppedHeights = [|0,0,0,0,0,0,0|]
    // let stopped = Set.empty<Position>
    
    let highestY (s:Set<Position>) =
        if s.IsEmpty then
            0L
        else
            (s |> Set.toSeq |> Seq.maxBy (fun p -> p.y)).y
    
    // Each rock appears so that its left edge is two units away from
    // the left wall and its bottom edge is three units above the
    // highest rock in the room (or the floor, if there isn't one).
    let nextStartingPosition stopped =
        {
            x = 2L
            y = (stopped |> highestY) + 4L
        }
    
    let initialShapeAsSet stopped s =
        let pos = nextStartingPosition stopped
        
        s |> positionModsByShape |> Seq.map (fun m ->
            {
                x = pos.x + (fst m |> int64)
                y = pos.y + (snd m |> int64)
            }
        )
        |> Set.ofSeq
        
    let maxYOfPositions s = (s |> Set.toSeq |> Seq.maxBy (fun p -> p.y)).y
    
    let drawIt (occupied:Set<Position>) (falling:Set<Position>) =
        let maxY = if falling.IsEmpty then maxYOfPositions occupied else maxYOfPositions falling
        
        wrapWith "\n" (seq { maxY .. -1L .. 0L } |> Seq.map (fun y ->
                let w = if y = 0L then "+" else "|"
                wrapWith w (seq { 0L .. 6L } |> Seq.map (fun x ->
                        let p = { x = x; y = y }
                        if y = 0L then
                            '-'
                        else if falling |> Set.contains p then
                            '@'
                        else if occupied |> Set.contains p then
                            '#'
                        else
                            '.'
                    ) |> Array.ofSeq |> String)
            )
            |> joinBy "\n")
        
    let drawStopped (stopped:Set<Position>) =
        drawIt stopped Set.empty<Position>
    
    type JetPush = | Left | Right
    
    module parser =
        let pLeft = %% %'<' -|> Left
        let pRight = %% %'>' -|> Right
        let pJetPattern = %% +.(%[pLeft;pRight] * qty[1..]) -|> Array.ofSeq
        let parseInput (input:string) = mustParse pJetPattern input
        
    let push dir shape =
        let newShape = shape |> Set.map (fun p ->
            match dir with
            | Left -> { p with x = p.x - 1L }
            | Right -> { p with x = p.x + 1L }
        )
        
        if newShape |> Set.exists (fun p -> p.x < 0 || p.x > 6) then
            shape
        else
            newShape
            
    let fallOne shape = shape |> Set.map (fun p -> { p with y = p.y - 1L })
    
    let intersectsWithGround s = s |> Set.exists (fun p -> p.y <= 0)
    let intersectsWithStopped stopped s = (s |> Set.intersect stopped).IsEmpty |> not
    let shapeOverlapsWithGroundOrStopped stopped s = intersectsWithGround s || intersectsWithStopped stopped s
    
    let dropUntilStopsAmong stopped (pushPattern:JetPush[]) pushIndex shape =
        let initialShape = shape |> initialShapeAsSet stopped
        let mutable pushing = false
        Seq.unfold (fun (prevShape,i) ->
            // drawIt stopped prevShape |> log id |> ignore
            pushing <- not pushing
            // log id (pushing,i) |> ignore
            if pushing then
                let nextPushIndex = (i + 1) % pushPattern.Length
                let nextPush = pushPattern[nextPushIndex]
                // nextPush |> log id |> ignore
                let pushedShape = push nextPush prevShape
                let nextShape = if shapeOverlapsWithGroundOrStopped stopped pushedShape then prevShape else pushedShape
                if shapeOverlapsWithGroundOrStopped stopped nextShape then
                    Some((prevShape,nextPushIndex), (prevShape,nextPushIndex))
                else
                    Some((nextShape,nextPushIndex), (nextShape,nextPushIndex))
            else
                // "Fall 1 unit" |> log id |> ignore
                let nextShape = prevShape |> fallOne
                if shapeOverlapsWithGroundOrStopped stopped nextShape then
                    None
                else
                    Some((nextShape,i), (nextShape,i))
        ) (initialShape, pushIndex)

    let minSetKeepHighestStoppedOnly stopped =
        seq { 0 .. 6 } |> Seq.collect (fun i ->
            let stoppedInColumnI = stopped |> Set.filter (fun p -> p.x = i)
            if stoppedInColumnI.IsEmpty then
                []
            else
                let maxY = stoppedInColumnI |> maxYOfPositions
                stoppedInColumnI |> Set.filter (fun p -> p.y > maxY - 6L) |> Set.toList
        ) |> Set.ofSeq

    let dropShapes (n:int64) (input:string) =
        let pushPattern = parser.parseInput input
        
        let initialJetPushIndex = -1 // haven't started yet
        let emptyChamber = Set.empty<Position>
        let emptyHighest = Set.empty<Position>
        
        Seq.unfold (fun (stopped, emptyHighest, shapeNumber, pushIndex) ->
                if shapeNumber = n then
                    None
                else
                    let n = (shapeNumber % (shapeSequence.Length |> int64)) |> int32
                    let nextShape = shapeSequence[n]
                    let stoppedShape,nextPushIndex = dropUntilStopsAmong emptyHighest pushPattern pushIndex nextShape |> Seq.last
                    let nextStopped = Set.union stopped stoppedShape
                    let minSet = nextStopped |> minSetKeepHighestStoppedOnly
                    Some(nextStopped, (nextStopped, minSet, shapeNumber + 1L, nextPushIndex))
            ) (emptyChamber, emptyHighest, 0L ,initialJetPushIndex)

    let part1_how_many_units_tall_will_the_tower_be_after_n_rocks_have_stopped_falling n (input:string) =
        let stopped = input |> dropShapes n
        stopped |> Seq.last |> maxYOfPositions
        
    let part2_how_many_units_tall_will_the_tower_be_after_lots_of_rocks_have_stopped_falling n (input:string) =
        part1_how_many_units_tall_will_the_tower_be_after_n_rocks_have_stopped_falling n input
        
    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Array.ofSeq
