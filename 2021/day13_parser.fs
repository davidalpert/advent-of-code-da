namespace AdventOfCode

module Origami =

  module AST =

    open System

    type Coordinate = {
      x:int
      y:int
    }
    with
      static member lift x y =
        { x = x; y = y; }

    type FoldDirection =
    | Up
    | Left

    type FoldInstruction = {
      direction: FoldDirection
      value: int
    }
    with
      static member liftAlongAxis direction value =
        { direction = direction; value = value; }

    type ThermalImage = {
      coordinates : Coordinate seq
      foldInstructions: FoldInstruction list
    }
    with
      static member lift coordinates foldInstructions =
        { coordinates = coordinates; foldInstructions = foldInstructions |> List.ofSeq; }
        
      member image.render =
        let max = {
          x = (image.coordinates |> Seq.map (fun c -> c.x) |> Seq.max);
          y = (image.coordinates |> Seq.map (fun c -> c.y) |> Seq.max);
        }

        let hasDot d =
          image.coordinates |> Seq.exists (fun c -> c = d)

        let coordinateToChar s =
          match hasDot s with
          | true -> '#'
          | false -> '.'

        seq { 0 .. max.y } |> Seq.map (fun y ->
          seq { 0 .. max.x } |> Seq.map (fun x ->
              coordinateToChar {x = x; y = y;}
            )
            |> Array.ofSeq
            |> String
          )
          |> String.concat "\n"

      member image.numberOfVisibleDots =
        image.coordinates |> Seq.length

      member image.foldNTimes n =
        let foldAgainst (cc:Coordinate seq) (f:FoldInstruction) =
          // printfn "folding: %A %d" f.direction f.value

          let transformAround v n = (0 - abs (n - v)) + v

          match f.direction with
          | Up -> cc |> Seq.map (fun c ->
              {
                x = c.x;
                y = transformAround f.value c.y;
              }
            )
          | Left -> cc |> Seq.map (fun c ->
              {
                x = transformAround f.value c.x;
                y = c.y;
              }
            )

        let instructionsToApply = image.foldInstructions[..(n-1)]
        let remainingInstructions = image.foldInstructions[(n-1)..]

        let foldedCoordinates =
          instructionsToApply
          |> Seq.fold foldAgainst (image.coordinates)
          |> Seq.distinct

        ThermalImage.lift foldedCoordinates remainingInstructions

      member image.fold =
        image.foldNTimes image.foldInstructions.Length

  module Parser =

    open System
    open FParsec
    open FParsec.Pipes
    open AST

    let ch = pchar
    let ws = spaces

    // trace helper to assist with debugging misbehaving parsers
    let (<!>) (p: Parser<_,_>) (label) : Parser<_,_> =
      fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

    let number =
      %% +.(digit * qty.[1..])
      -|> fun digits -> digits
                        |> Array.ofSeq
                        |> String
                        |> Int32.Parse
      // <!> "number"

    let coordinate =
      %% +.number -- ',' -- +.number -- '\n'
      -|> Coordinate.lift
      // <!> "coordinate"

    let foldAlong (c:char) a =
      %% "fold along" -- ws -? c -- '=' -- +.number -- ws
      -|> FoldInstruction.liftAlongAxis a

    let foldInstruction =
      %[
        foldAlong 'y' Up;
        foldAlong 'x' Left;
      ]

    let pThermalImage =
      %% ws
      -- +.(coordinate * qty.[1..])
      -- ws
      -- +.(foldInstruction * qty.[1..])
      -|> ThermalImage.lift

    let parse (input:string) =
      match run pThermalImage input with
      | Success(r, _, _)   -> Result.Ok(r)
      | Failure(errorMsg, _, _) -> Result.Error(errorMsg)