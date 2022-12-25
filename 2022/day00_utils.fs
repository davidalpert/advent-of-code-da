namespace AdventOfCode

open System

module utils =
  open FParsec

  // https://rolfsuter.ch/code/f-transpose-a-list-of-lists-transpose-2d-matrix/
  let rec transpose =
    function
    | (_ :: _) :: _ as M ->
        List.map List.head M
        :: transpose (List.map List.tail M)
    | _ -> []

  // borrowed from Kirk
  let log transformer value =
    printfn $"%A{transformer value}"
    value

  let ws = spaces

  let mustParse p (input:string) =
    match run p (input.Trim()) with
    | Success(r, _, _) -> r
    | Failure(errorMsg, _, _) -> failwith errorMsg

  // trace helper to assist with debugging misbehaving parsers
  let (<!>) (p: Parser<_,_>) (label) : Parser<_,_> =
    fun stream ->
      printfn "%A: Entering %s" stream.Position label
      let reply = p stream
      printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
      reply

  let firstPairFromArray ss =
    match Array.length ss with
    | l when l >= 2 -> (ss[0], ss[1])
    | _ -> failwithf $"firstPairFromArray requires at least 2 elements; found %d{ss |> Array.length}"

  let asSet (first, last) =
    seq { first .. last } |> Set.ofSeq

  let eitherOr fn (a, b) =
    (a |> fn b) || (b |> fn a)

  let spread fn (a, b) =
    (fn a, fn b)

  let contains a b =
    Set.isSubset a b

  let overlaps a b =
    (Set.intersect a b) <> Set.empty

  // minTuple creates a new tuple with the min fst and snd values
  let minTuple pair =
    pair |> Seq.minBy fst |> fst, pair |> Seq.minBy snd |> snd

  // maxTuple creates a new tuple with the max fst and snd values
  let maxTuple pair =
    pair |> Seq.maxBy fst |> fst, pair |> Seq.maxBy snd |> snd

  let splitBy (sep: string) (input: string) =
    input.Split(sep)

  let joinBy (sep:string) (values:string seq) =
     String.Join(sep, values)

  let wrapWith (padding:string) (s:string) = padding + s + padding

  let trim (s:string) = s.Trim()

  let flattenPairsArray pairs =
    pairs |> Array.collect (fun pair -> [|fst pair; snd pair|])
    
  // unfolds an infinitely repeating sequence of pattern, wrapping
  // around to the beginning once the end has been reached
  let repeatingSequenceOf (pattern:'a[]) =
    let fn index =
      let nextIndex = (index + 1) % pattern.Length
      Some(pattern[index], nextIndex)
    Seq.unfold fn 0
     
  // a generic type to wrap some data together with a custom comparer
  [<CustomEquality; CustomComparison>]
  type SortableData<'a> when 'a : equality = {
     data: 'a
     comparer: 'a * 'a -> bool
  }
  with
        interface IEquatable<SortableData<'a>> with
            member this.Equals other = other.data = this.data
  
        override this.Equals other =
            match other with
            | :? SortableData<'a> as p -> (this :> IEquatable<_>).Equals p
            | _ -> false
            
        override this.GetHashCode () = this.data.GetHashCode()
        
        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? SortableData<'a> as p -> (this :> IComparable<_>).CompareTo p
                | _ -> -1
                
        interface IComparable<SortableData<'a>> with
            member this.CompareTo other =
                match this.comparer (this.data, other.data) with
                | true -> -1
                | false -> 1
                
  let toSortableData comparer data =
    {
      data = data
      comparer = comparer
    }

  // source: https://rosettacode.org/wiki/Dijkstra%27s_algorithm#F#
  // --------------------------------------------------------------
  //Dijkstra's algorithm: Nigel Galloway, August 5th., 2018
  [<CustomEquality;CustomComparison>]
  type Dijkstra<'N,'G when 'G:comparison> =
    {toN:'N;cost:Option<'G>;fromN:'N}
  with
     override g.Equals n = match n with | :? Dijkstra<'N,'G> as n-> n.cost = g.cost | _ -> false
     override g.GetHashCode() = hash g.cost
     interface System.IComparable with
       member n.CompareTo g =
         match g with
         | :? Dijkstra<'N,'G> as n when n.cost=None -> (-1)
         | :? Dijkstra<'N,'G>      when n.cost=None -> 1
         | :? Dijkstra<'N,'G> as g                  -> compare n.cost g.cost
         | _-> invalidArg "n" "expecting type Dijkstra<'N,'G>"
                                            
  let inline Dijkstra N G y =
    let rec fN l f=
      if List.isEmpty l then f
      else let n=List.min l
           if n.cost=None then f else
           fN(l|>List.choose(fun n'->if n'.toN=n.toN then None else match n.cost,n'.cost,Map.tryFind (n.toN,n'.toN) G with
                                                                    |Some g,None,Some wg                ->Some {toN=n'.toN;cost=Some(g+wg);fromN=n.toN}
                                                                    |Some g,Some g',Some wg when g+wg<g'->Some {toN=n'.toN;cost=Some(g+wg);fromN=n.toN}
                                                                    |_                                  ->Some n'))((n.fromN,n.toN)::f)
    let r = fN (N|>List.map(fun n->{toN=n;cost=(Map.tryFind(y,n)G);fromN=y})) []
    (fun n->let rec fN z l=match List.tryFind(fun (_,g)->g=z) r with
                           |Some(n',g') when y=n'->Some(n'::g'::l)
                           |Some(n',g')          ->fN n' (g'::l)
                           |_                    ->None
            fN n [])
  // --------------------------------------------------------------
