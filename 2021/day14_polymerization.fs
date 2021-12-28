namespace AdventOfCode

module Polymerization =

  open System
  open System.Collections.Generic

  type instructions = {
    template: char array
    rules: IDictionary<char*char,char>
  }
  with
    member x.toString = x.template |> String

    member x.step =
      let chars = x.template
      let first = chars[0]
      let resta =
        x.template
        |> Seq.windowed 2
        |> Seq.map (fun cc -> cc[0],cc[1])
        |> Seq.map (fun cc ->
          match x.rules.TryGetValue cc with
          | true, value -> (value, snd cc)
          | false, _    -> cc
        )

      let rest =
        resta
        |> Seq.map (fun cc -> [fst cc; snd cc])
        |> Seq.concat
        |> List.ofSeq

      let merged = ( first :: rest ) |> Array.ofList

      { x with template = merged }

    member x.afterNSteps n =
      let applyRules (i:instructions) n =
        // printfn "step: %d" n
        i.step

      seq { 1 .. n } |> Seq.fold applyRules x

    member x.polymerAfterNSteps n =
      (x.afterNSteps n).template |> String

    member x.differenceBetweenMaxAndMinNumberOfOccurances =
      let y = x.template
              |> Array.groupBy (fun c -> c)
              |> Array.map (fun (c,cc) -> (c,cc.Length))
              |> Array.sortByDescending (fun (c,n) -> n)

      let (maxC,maxN) = y |> Array.head
      let (minC,minN) = y |> Array.last

      maxN - minN

  let parse (input:string) =
    let parts = input.Trim().Split("\n\n")
    let template = parts[0].Trim().ToCharArray()
    let rules = parts[1].Split("\n")
                |> Array.map (fun s -> s.Trim())
                |> Array.map (fun s -> s.Split(" -> "))
                |> Array.map (fun ss -> 
                  let parts = ss[0].ToCharArray()
                  let key = parts[0],parts[1]
                  let value = ss[1].ToCharArray()[0]
                  (key,value)
                )
                |> dict

    { template = template; rules = rules; }
