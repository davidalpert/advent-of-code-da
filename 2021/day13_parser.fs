namespace AdventOfCode

module Origami =

  module AST =

    type Coordinate = {
      x:int
      y:int
    }
    with
      static member lift x y =
        { x = x; y = y; }

    type Axis =
    | XAxis
    | YAxis

    type FoldInstruction = {
      axis: Axis
      value: int
    }
    with
      static member liftAlongAxis axis value =
        { axis = axis; value = value; }

    type ThermalImage = {
      coordinates : Coordinate seq
      foldInstructions: FoldInstruction seq
    }
    with
      static member lift coordinates foldInstructions =
        { coordinates = coordinates; foldInstructions = foldInstructions; }

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
      %% "fold along" -- ws -? c -- '=' -- +.number
      -|> FoldInstruction.liftAlongAxis a

    let foldInstruction =
      %[
        foldAlong 'x' XAxis;
        foldAlong 'y' YAxis;
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