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

  // https://rosettacode.org/wiki/Dijkstra%27s_algorithm#Dijkstra.27s_algorithm
  //Dijkstra's algorithm: Nigel Galloway, August 5th., 2018
  [<CustomEquality;CustomComparison>]
  type Dijkstra<'N,'G when 'G:comparison> =
    {
      toN:'N;
      cost:Option<'G>;
      fromN:'N
    }

    override g.Equals n =
      match n with
      | :? Dijkstra<'N,'G> as n -> n.cost = g.cost
      | _ -> false

    override g.GetHashCode() = hash g.cost

    interface System.IComparable with
      member n.CompareTo g =
        match g with
        | :? Dijkstra<'N,'G> as n when n.cost = None -> (-1)
        | :? Dijkstra<'N,'G>      when n.cost = None -> 1
        | :? Dijkstra<'N,'G> as g                  -> compare n.cost g.cost
        | _-> invalidArg "n" "expecting type Dijkstra<'N,'G>"

  let inline Dijkstra N G y =
    let rec fN l f =
      if List.isEmpty l then
        f
      else
        let n = List.min l

        if n.cost = None then
          f
        else
          fN(
            l
            |> List.choose(
              fun n' ->
                if n'.toN = n.toN then
                  None
                else
                  match n.cost,n'.cost,Map.tryFind (n.toN,n'.toN) G with
                  | Some g,None,Some wg ->
                    Some {
                      toN = n'.toN;
                      cost = Some(g+wg);
                      fromN = n.toN
                    }
                  | Some g,Some g',Some wg when g+wg<g' ->
                    Some {
                      toN=n'.toN;
                      cost=Some(g+wg);
                      fromN=n.toN
                    }
                  | _ ->
                    Some n'
              )
          ) ((n.fromN,n.toN)::f)

    let r = fN (N |> List.map (fun n -> { toN=n; cost=(Map.tryFind(y,n)G); fromN=y })) []

    (fun n->
      let rec fN z l =
        match List.tryFind(fun (_,g)->g=z) r with
        | Some(n',g') when y = n'-> Some(n'::g'::l)
        | Some(n',g')            -> fN n' (g'::l)
        | _                               -> None
      fN n [])

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

    // let n = vertices.Length

    let combinations = permuteAll (vertices |> List.ofArray)

    printfn "---"
    printfn "%A" combinations
    printfn "---"

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

    // let shortestPathFromEachStartingCity =
    //   vertices
    //   |> Array.map (fun startingCity ->
    //     vertices
    //     |> Array.filter (fun endingCity -> endingCity <> startingCity)
    //     |> List.map (fun endingCity ->
    //       (Dijkstra vertices edgeMap startingCity) endingCity
    //     )
    //     |> List.choose id
    //   )

    // printfn "%A" shortestPathFromEachStartingCity

    a
