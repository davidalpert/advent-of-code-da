namespace AdventOfCode

module ProbeLauncher =

  type Position = { x : int ; y : int }
  with
    member p.isPast (area:TargetArea) =
      p.x > area.p2.x || p.y < area.p1.y

    static member initial = { x = 0; y = 0; }

  and Velocity = { vx : int ; vy : int }

  and Trajectory = (Position * Velocity) list

  and TargetArea = {
    p1: Position
    p2: Position
  }
  with
    member area.lowestDepth = min (area.p1.y) (area.p2.y)
    member area.greatestHeight = max (area.p1.y) (area.p2.y)
    member area.closestEdge = area.p1.x
    member area.farthestEdge = area.p2.x

    member area.contains (p:Position) =
      area.p1.x <= p.x && p.x <= area.p2.x &&
      area.p1.y <= p.y && p.y <= area.p2.y

    member area.mapTrajectory (vInitial:Velocity) : Trajectory =
      let next (p:Position) (v:Velocity) =
        let pNext = {
          x = p.x + v.vx; // The probe's x position increases by its x velocity.
          y = p.y + v.vy; // The probe's y position increases by its y velocity.
        }

        let vNext = {
          // Due to drag, the probe's x velocity changes by 1 toward the value 0; that is...
          vx = match sign (v.vx) with
               |  1 -> v.vx - 1 // ...it decreases by 1 if it is greater than 0,
               | -1 -> v.vy + 1 // ...increases by 1 if it is less than 0, or
               |  0 -> 0        // ...does not change if it is already 0.
               |  _ -> failwith "invalid sign"
          vy = v.vy - 1; // Due to gravity, the probe's y velocity decreases by 1. 
        }

        (pNext, vNext)

      // collect all the points until in the trench or past it
      
      Seq.unfold (fun (p:Position, v:Velocity) ->
        if p.isPast area then
          None
        else
          Some((p,v), (next p v))
      ) (Position.initial, vInitial)
      |> List.ofSeq

    member area.endsInside (trajectory:Trajectory) =
      let pFinal =
        trajectory
        |> Seq.last
        |> fst

      let inside = area.contains pFinal

      // if inside then
      //   let (_,vInitial) = trajectory.Head
      //   printfn "%d,%d ends inside (%A)" vInitial.vx vInitial.vy inside

      inside

    member area.allPossibleVelocitiesLessThan (initialVY:int) =
      Seq.unfold (fun (v:Velocity) ->
        if (v.vy < area.greatestHeight && v.vx > area.farthestEdge) then
          None
        else
          let vNext =
            if v.vy < area.greatestHeight then
              {
                vx = v.vx + 1;
                vy = initialVY;
              }
            else
              {
                vx = v.vx;
                vy = v.vy - 1;
              }

          // printfn "v: %d,%d" v.vx v.vy
          Some(v, vNext)
      ) { vx = 0; vy = initialVY; }

    member area.render (vInitial:Velocity) =
      let trajectory = vInitial |> area.mapTrajectory |> List.map fst
      let yy = trajectory |> List.map (fun p -> p.y)
      let maxHeight = max 0 (max (yy |> List.max) (area.greatestHeight))
      let maxDepth = min (yy |> List.min) (area.lowestDepth)

      let txt =
        seq { maxHeight .. -1 .. maxDepth }
        |> Seq.map (fun y ->
          seq { 0 .. area.farthestEdge }
          |> Seq.map (fun x ->
            let p = { x = x; y = y; }
            if p = Position.initial then
              "S"
            else if trajectory |> Seq.exists (fun q -> q = p) then
              "#"
            else if area.contains p then
              "T"
            else 
              "."
          )
          |> String.concat ""
        )
        |> String.concat "\n"

      sprintf "\n%s\n" txt

  let initialVelocity (trajectory:Trajectory) =
    let _,vInitial = trajectory.Head
    vInitial

  let highestPoint (trajectory:Trajectory) =
    let vInitial = trajectory |> initialVelocity

    let highest =
      trajectory
      |> Seq.maxBy (fun (p,_) -> p.y)

    let (p,_) = highest

    // printfn "%d,%d highest: %d" vInitial.vx vInitial.vy p.y

    highest

  let greatestHeight (trajectory:Trajectory) =
    let (p,_) = trajectory |> highestPoint
    p.y

module TargetAreaParser =
    open FParsec
    open FParsec.Pipes
    open ProbeLauncher

    let sequence =
      %% +.p<int> -- ".." -- +.p<int>
      -|> fun a b -> (a,b)

    let pTargetArea =
      %% "target area: x=" -- +.sequence -- ", y=" -- +.sequence
      -|> fun xs ys -> {
        p1 = { x = fst xs; y = fst ys };
        p2 = { x = snd xs; y = snd ys }
      }

    let parse (input:string) =
      match run pTargetArea input with
      | Success(r, _, _)   -> Result.Ok(r)
      | Failure(errorMsg, _, _) -> Result.Error(errorMsg)
