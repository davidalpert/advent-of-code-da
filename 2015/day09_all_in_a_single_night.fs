namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module AllInASingleDay =

  open FParsec
  open FParsec.Pipes

  let ch = pchar
  let ws = spaces

  type Segment = {
    Start: string
    End: string
    Distance: int
  }

  let pCity = identifier (IdentifierOptions())

  let pEdge =
    // London to Dublin = 464
    %% +.(pCity) -- ws -- "to" -- ws -- +.(pCity) -- ws -- '=' -- ws -- +.(%p<int>)
    -|> fun fromCity toCity distance ->
      {
        Start = fromCity;
        End = toCity;
        Distance = distance;
      }

  let parseRawEdge (input:string) =
    match run pEdge input with
    | Success(r, _, _) -> r
    | Failure(msg, _, _) -> raise (System.Exception(msg))

  let calculatedShortestPathToVisitAllNodes (rawEdge:seq<string>) =
    let edges = 
      rawEdge
      |> Seq.map parseRawEdge

    let vertices =
      edges
      |> Seq.collect (fun s -> [|s.Start; s.End|])
      |> Seq.distinct
      |> Seq.sort
      |> Array.ofSeq

    let combinations = permuteAll (vertices |> List.ofArray)

    // printfn "---"
    // printfn "%A" combinations
    // printfn "---"

    let edgeMap =
      edges
      |> Seq.collect (fun e ->
        [|
          ((e.Start,e.End),e.Distance)
          ((e.End,e.Start),e.Distance)
        |]
      )
      |> Map.ofSeq

    let a =
      combinations
      |> Seq.map (fun path ->
        path
        |> Array.ofList
        |> Array.windowed 2
        |> Array.map (fun nodes ->
          let seg = (nodes.[0], nodes.[1])
          if edgeMap.ContainsKey(seg) then
            edgeMap.[seg]
          else
            raise (System.Exception(sprintf "cannot find cost of %A" seg))
        )
        |> Array.sum
      )
      |> Seq.min

    a
