namespace AdventOfCode

module Input =
  let splitToTrimmedLines (input:string) =
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())

  let splitToTrimmedLinesVerbose (input:string) =
    printfn "input: %A" input
    printfn "input.trimmed: %A" (input.Trim())
    printfn "input.trimmed.split: %A" (input.Trim().Split("\n"))
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())

  let day01sample = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

  let day01data = """
"""

  let lastData = ""
