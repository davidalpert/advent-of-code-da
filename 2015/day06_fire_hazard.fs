namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module FireHazard =

  open System

  type BulbLocation = {
    x : int
    y : int
  }

  type Instruction =
  | TurnOn of BulbLocation*BulbLocation
  | TurnOff of BulbLocation*BulbLocation
  | Toggle of BulbLocation*BulbLocation
  | Unrecognized of string
  with
    static member fromString (s:string) =
      match s with
      | Regex @"turn off (\d+),(\d+) through (\d+),(\d+)" [ x1s; y1s; x2s; y2s ] ->
        TurnOff({x = x1s |> int; y = y1s |> int},{x = x2s |> int; y = y2s |> int})
      | Regex @"turn on (\d+),(\d+) through (\d+),(\d+)" [ x1s; y1s; x2s; y2s ] ->
        TurnOn({x = x1s |> int; y = y1s |> int},{x = x2s |> int; y = y2s |> int})
      | Regex @"toggle (\d+),(\d+) through (\d+),(\d+)" [ x1s; y1s; x2s; y2s ] ->
        Toggle({x = x1s |> int; y = y1s |> int},{x = x2s |> int; y = y2s |> int})
      | _ -> Unrecognized(s)

  let toInstructions (rawInput:string) : seq<Instruction> =
    rawInput
    |> splitToTrimmedLines
    |> Seq.map Instruction.fromString

  let applyInstructions (ii:seq<Instruction>) =
    let folder (lit:Set<BulbLocation>) (i:Instruction) =
      match i with
      | TurnOn(l1,l2) ->
        let turningOn = seq {
          for x in l1.x .. l2.x do
          for y in l1.y .. l2.y do
          yield { x = x; y = y}
        }
        Set.union lit (turningOn |> Set.ofSeq)

      | TurnOff(l1,l2) ->
        let turningOff = seq {
          for x in l1.x .. l2.x do
          for y in l1.y .. l2.y do
          yield { x = x; y = y}
        }
        Set.difference lit (turningOff |> Set.ofSeq)

      | Toggle(l1,l2) ->
        let toggling = seq {
          for x in l1.x .. l2.x do
          for y in l1.y .. l2.y do
          yield { x = x; y = y}
        }

        let togglingSet = toggling |> Set.ofSeq

        let remainingOn = Set.difference lit togglingSet

        let turningOn = Set.difference togglingSet lit  

        Set.union remainingOn turningOn

      | Unrecognized(_) -> lit
    
    ii
    |> Seq.fold folder Set.empty<BulbLocation>

  let applyInstructions2 (ii:seq<Instruction>) =
    let brightness = System.Collections.Generic.Dictionary<BulbLocation,int>()

    let calculateNewBrightness (oldBrightness:int) (i:Instruction) =
      match i with
      | TurnOn(_,_) -> oldBrightness + 1
      | TurnOff(_,_) -> Math.Max(0,(oldBrightness - 1))
      | Toggle(_,_) -> oldBrightness + 2
      | Unrecognized(_) -> oldBrightness
  
    let applyBrightnessChange (i:Instruction) (l:BulbLocation) =
      match brightness.ContainsKey l with
      | true -> brightness.[l] <- calculateNewBrightness (brightness.[l]) i
      | false -> brightness.Add(l, (calculateNewBrightness 0 i))

    let apply (i:Instruction) =
      let range =
        match i with
        | TurnOn(l1,l2) -> seq { for x in l1.x .. l2.x do for y in l1.y .. l2.y do yield { x = x; y = y} }
        | TurnOff(l1,l2) -> seq { for x in l1.x .. l2.x do for y in l1.y .. l2.y do yield { x = x; y = y} }
        | Toggle(l1,l2) -> seq { for x in l1.x .. l2.x do for y in l1.y .. l2.y do yield { x = x; y = y} }
        | Unrecognized(_) -> [||]

      range
      |> Seq.iter (applyBrightnessChange i)
    
    ii
    |> Seq.iter apply

    brightness.Values