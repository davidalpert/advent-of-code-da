namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module AssemblyRequired =

  open System
  open System.Collections.Generic

  type wire = string

  type CircuitComponent =
  | Signal of uint16 * wire
  | DirectRoutingFrom of wire * wire
  | BitwiseAnd of wire * wire * wire
  | BitwiseNumericAnd of uint16 * wire * wire
  | BitwiseOr of wire * wire * wire
  | LeftShift of wire * int * wire
  | RightShift of wire * int * wire
  | BitwiseComplement of wire * wire
  with
    static member cache = Dictionary<wire,uint16>()

    member this.valueOf (cc:IDictionary<wire,CircuitComponent>) =
      // printfn "valueOf: %A" this
      let cacheOrLookup (w:wire) =
        if CircuitComponent.cache.ContainsKey(w) then
          printfn "cachedValueOf %A" w 
          CircuitComponent.cache.[w]
        else
          let v = (cc.[w].valueOf cc)
          CircuitComponent.cache.Add(w,v)
          v

      match this with
      | Signal(v,w) -> v
      | DirectRoutingFrom(x,w) -> cacheOrLookup x
      | BitwiseAnd(x,y,w) -> (cacheOrLookup x) &&& (cacheOrLookup y)
      | BitwiseNumericAnd(x,y,w) -> x &&& (cacheOrLookup y)
      | BitwiseOr(x,y,w) -> (cacheOrLookup x) ||| (cacheOrLookup y)
      | LeftShift(x,v,w) -> (cacheOrLookup x) <<< v
      | RightShift(x,v,w) -> (cacheOrLookup x) >>> v
      | BitwiseComplement(x,w) -> ~~~ (cacheOrLookup x)

  let valueSentToWire (w:string) (cc:IDictionary<wire,CircuitComponent>) =
    if cc.ContainsKey(w) then
      cc.[w].valueOf cc
    else
      0 |> uint16

  let assembleCircuit (instructions:seq<string>) =
    let cc = SortedDictionary<wire,CircuitComponent>()

    instructions
    |> Seq.iter (fun i ->
      match i with
      | Regex @"^(\d+) -> (\w+)" [ v; w] -> cc.[w] <- Signal(v |> uint16,w)
      | Regex @"^(\w+) -> (\w+)" [ x; w] -> cc.[w] <- DirectRoutingFrom(x,w)
      | Regex @"^(\d+) AND (\w+) -> (\w+)" [ x; y; w] -> cc.[w] <- BitwiseNumericAnd(x |> uint16,y,w)
      | Regex @"^(\w+) AND (\w+) -> (\w+)" [ x; y; w] -> cc.[w] <- BitwiseAnd(x,y,w)
      | Regex @"^(\w+) OR (\w+) -> (\w+)" [ x; y; w] -> cc.[w] <- BitwiseOr(x,y,w)
      | Regex @"^(\w+) LSHIFT (\d+) -> (\w+)" [ x; v; w] -> cc.[w] <- LeftShift(x,v |> int,w)
      | Regex @"^(\w+) RSHIFT (\d+) -> (\w+)" [ x; v; w] -> cc.[w] <- RightShift(x,v |> int,w)
      | Regex @"^NOT (\w+) -> (\w+)" [ x; w ] -> cc.[w] <- BitwiseComplement(x,w)
      | _ -> printfn "ignoring: %s" i
    )

    cc
