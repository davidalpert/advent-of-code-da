namespace AdventOfCode

module Polymerization =

  open System
  open System.Collections.Generic

  type instructions = {
    template: string
    rules: IDictionary<string,char>
  }
  with
    member x.step =
      let chars = x.template.ToCharArray()
      let first = chars[0]
      let rest =
        x.template.ToCharArray()
        |> Seq.windowed 2
        |> Seq.map (fun cc ->
          match x.rules.TryGetValue (cc |> String) with
          | true, value -> [value;cc[1]]
          | false, _    -> [cc[1]]
        )
        |> Seq.concat
        |> List.ofSeq

      let merged = ( first :: rest ) |> Array.ofList |> String

      { x with template = merged }

    member x.afterNSteps n =
      let applyRules (i:instructions) n = i.step
      seq { 1 .. n } |> Seq.fold applyRules x

    member x.polymerAfterNSteps n =
      (x.afterNSteps n).template

    member x.differenceBetweenMaxAndMinNumberOfOccurances =
      let y = x.template.ToCharArray()
              |> Array.groupBy (fun c -> c)
              |> Array.map (fun (c,cc) -> (c,cc.Length))
              |> Array.sortByDescending (fun (c,n) -> n)

      let (maxC,maxN) = y |> Array.head
      let (minC,minN) = y |> Array.last

      maxN - minN

  let parse (input:string) =
    let parts = input.Trim().Split("\n\n")
    let template = parts[0].Trim()
    let rules = parts[1].Split("\n")
                |> Array.map (fun s -> s.Trim())
                |> Array.map (fun s -> s.Split(" -> "))
                |> Array.map (fun ss -> (ss[0],ss[1].ToCharArray()[0]))
                |> dict

    { template = template; rules = rules; }
