namespace AdventOfCode

module Hydrothermals =

  open AdventOfCode.Input

  type Point = {
    x: int
    y: int
  }

  type LineSegment = {
    p1: Point
    p2: Point
  }
  with
    member s.covers (p:Point) =
      (((s.p1.x < s.p2.x) && (s.p1.x <= p.x && p.x <= s.p2.x )) || ((s.p2.x <= s.p1.x) && (s.p2.x <= p.x && p.x <= s.p1.x ))) && 
      (((s.p1.y < s.p2.y) && (s.p1.y <= p.y && p.y <= s.p2.y )) || ((s.p2.y <= s.p1.y) && (s.p2.y <= p.y && p.y <= s.p1.y )))

    member s.coveredHorizontally =
      if s.p1.y = s.p2.y then
        let m =
          if s.p1.x < s.p1.y then
            1
          else
            -1

        seq { s.p1.x .. m .. s.p2.x }
        |> Seq.map (fun x -> { x = x; y = s.p1.y; })
        |> List.ofSeq
      else
        []

    member s.coveredVertically =
      if s.p1.x = s.p2.x then
        let m =
          if s.p1.y <= s.p2.y then
            1
          else
            -1

        seq { s.p1.y .. m .. s.p2.y }
        |> Seq.map (fun y -> { x = s.p1.x; y = y })
        |> List.ofSeq
      else
        []

    member s.coveredHorizontallyOrVertically =
      List.concat [s.coveredHorizontally; s.coveredVertically]

  let ventsFromInput (input: string) =
    splitToTrimmedLines input
    |> Seq.map (fun s -> s.Split " -> ")
    |> Seq.map Seq.ofArray
    |> Seq.map (fun line ->
      let points =
        line
        |> Seq.map (fun part ->
          let coordinates = part.Split(",")
          {
            x = coordinates.[0] |> int;
            y = coordinates.[1] |> int;
          }
        )
        |> Array.ofSeq
      {
        p1 = points[0];
        p2 = points[1];
      }
    )
    |> Seq.map (fun l ->
      printfn "%A" l
      l
    )

  type VentedPoint = {
    p:Point
    intensity:int
  }

  type ThermalScan(vents:LineSegment list) =
    let vents = vents

    let accumulateVentedPointCount (ventedPoints:VentedPoint list) (p:Point) =
      let pointAlreadyVented =
        ventedPoints
        |> List.tryFind (fun v -> v.p = p)

      match pointAlreadyVented with
      | Some(_) ->
        ventedPoints
        |> List.map (fun v ->
          if v.p = p then
            { v with intensity = v.intensity + 1 }
          else
            v
        )
      | None -> List.concat [ventedPoints; [ { p = p; intensity = 1; }]]

    let ventedPoints =
      vents
      |> List.map (fun v -> v.coveredHorizontallyOrVertically)
      |> List.concat
      |> List.fold accumulateVentedPointCount []

    
