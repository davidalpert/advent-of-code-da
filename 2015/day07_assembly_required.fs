namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module AssemblyRequired =

  open System
  open System.Collections.Generic

  let assembleCircuit (instructions:seq<string>) =
    let cc = SortedDictionary<string,uint16>()

    let safeGetValue (c:string) =
      if cc.ContainsKey(c) then
        // printfn "getting value of '%s': %d" c (cc.[c])
        cc.[c]
      else
        0 |> uint16

    instructions
    |> Seq.iter (fun i ->
      match i with
      | Regex @"^(\d+) -> (\w)" [ v; c] -> cc.[c] <- (v |> uint16)
      | Regex @"^(\w+) AND (\w+) -> (\w+)" [ x; y; d] -> cc.[d] <- ((x |> safeGetValue) &&& (y |> safeGetValue))
      | Regex @"^(\w+) OR (\w+) -> (\w+)" [ x; y; d] -> cc.[d] <- ((x |> safeGetValue) ||| (y |> safeGetValue))
      | Regex @"^(\w+) LSHIFT (\d+) -> (\w+)" [ c; v; d] -> cc.[d] <- ((c |> safeGetValue) <<< (v |> int))
      | Regex @"^(\w+) RSHIFT (\d+) -> (\w+)" [ c; v; d] -> cc.[d] <- ((c |> safeGetValue) >>> (v |> int))
      | Regex @"^NOT (\w+) -> (\w+)" [ x; d ] -> cc.[d] <- (~~~ (x |> safeGetValue))
      | _ -> printfn "ignoring: %s" i
    )

    cc
