namespace AdventOfCode

module Hydrothermals =

  open AdventOfCode.Input
  open System.Collections.Generic

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

    member s.isVertical =
      s.p1.x = s.p2.x

    member s.isHorizontal =
      s.p1.y = s.p2.y

    member s.coveredHorizontally =
      if s.isHorizontal then
        let m =
          if s.p1.x < s.p2.x then
            1
          else
            -1

        seq { s.p1.x .. m .. s.p2.x }
        |> Seq.map (fun x -> { x = x; y = s.p1.y; })
        |> List.ofSeq
      else
        []

    member s.coveredVertically =
      if s.isVertical then
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

    member s.coveredDiagonally =
      if s.isHorizontal || s.isVertical then
        []
      else
        let deltaX = s.p2.x - s.p1.x
        let xm = if deltaX > 0 then 1 else -1
        let deltatY = s.p2.y - s.p1.y
        let ym = if deltatY > 0 then 1 else -1

        let xs = seq { s.p1.x .. xm .. s.p2.x }
        let ys = seq { s.p1.y .. ym .. s.p2.y }

        Seq.zip xs ys
        |> Seq.map (fun (x, y) -> { x = x; y = y; })
        |> List.ofSeq
  
    member s.coveredHorizontallyOrVertically =
      List.concat [s.coveredHorizontally; s.coveredVertically]

    member s.coveredHorizontallyOrVerticallyOrDiagnoally =
      List.concat [s.coveredHorizontally; s.coveredVertically; s.coveredDiagonally]

  let segmentIsHorizontal (s:LineSegment) =
    s.isHorizontal

  let segmentIsVertical (s:LineSegment) =
    s.isVertical

  let segmentIsHorizontalOrVertical (s:LineSegment) =
    s.isHorizontal || s.isVertical

  let ventsFromInput (input: string) =
    let pairToPoint (pair:string) =
        let coordinates = pair.Split(",")
        {
          x = coordinates.[0] |> int;
          y = coordinates.[1] |> int;
        }

    let pointsToSegment (points:Point[]) =
      {
        p1 = points[0];
        p2 = points[1];
      }

    let lineToSegment (line:string) =
      line
      |> (fun s -> s.Split " -> ")
      |> Array.map pairToPoint
      |> pointsToSegment

    splitToTrimmedLines input
    |> Seq.map lineToSegment
    |> List.ofSeq

  type VentedPoint = {
    p:Point
    intensity:int
  }

  type ThermalScan(vents:LineSegment list) =
    let vents = vents

    let collectVentedPoints (projection:LineSegment -> Point list) =
      let dict = new Dictionary<Point, int>()

      let apply (p:Point) =
        if dict.ContainsKey(p) then
          dict[p] <- dict[p] + 1
        else
          dict[p] <- 1

      vents
      |> List.map projection
      |> List.concat
      |> List.iter apply

      dict

    member x.ventedPoints =
      collectVentedPoints (fun v -> v.coveredHorizontallyOrVertically)

    member x.ventedPoints2 =
      collectVentedPoints (fun v -> v.coveredHorizontallyOrVerticallyOrDiagnoally)

  let diagram (ventedPoints:Dictionary<Point,int>) =
    let maxX = ventedPoints.Keys |> Seq.map (fun p -> p.x) |> Seq.max
    let maxY = ventedPoints.Keys |> Seq.map (fun p -> p.y) |> Seq.max
    let asString (p:Point) =
      if ventedPoints.ContainsKey(p) then
        ventedPoints[p].ToString()
      else
        "."
    seq { 0 .. maxY }
    |> Seq.map (fun y ->
      seq { 0 .. maxX }
      |> Seq.map (fun x -> { x = x; y = y; })
      |> Seq.map asString
      |> Seq.fold (fun ss s -> ss + s) ""
      // |> Seq.map asString
    )
    |> Seq.fold (fun ss line -> ss + line + "\n") ""

