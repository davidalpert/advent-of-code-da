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

  let valueSentToWire (s:string) (cc:IDictionary<wire,CircuitComponent>) =
    let rec valueOf (ss:string) =
      let lookupAndCache (www:wire) =
        let v = valueOf www
        // printfn "caching '%s': %d" www v
        cc.[www] <- Signal(v, www)
        v

      match cc.[ss] with
      | Signal(v,w) -> v
      | DirectRoutingFrom(x,w) -> lookupAndCache x
      | BitwiseAnd(x,y,w) -> (lookupAndCache x) &&& (lookupAndCache y)
      | BitwiseNumericAnd(x,y,w) -> x &&& (lookupAndCache y)
      | BitwiseOr(x,y,w) -> (lookupAndCache x) ||| (lookupAndCache y)
      | LeftShift(x,v,w) -> (lookupAndCache x) <<< v
      | RightShift(x,v,w) -> (lookupAndCache x) >>> v
      | BitwiseComplement(x,w) -> ~~~ (lookupAndCache x)

    if cc.ContainsKey(s) then
      valueOf s
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

  let replaceSignalSentToB (overriddenValueOfB:uint16) (instructions:seq<string>) =
    instructions
    |> Seq.map (fun i ->
      // printfn "preprocessing: %s" i
      match i with 
      | Regex @"^(\d+) -> b$" [ v] ->
        let newI = sprintf "%d -> b" overriddenValueOfB
        // printfn "overriding '%s' with '%s'" i newI
        newI
      | _ -> i
    )