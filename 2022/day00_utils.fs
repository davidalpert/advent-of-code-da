namespace AdventOfCode

open System

module utils =
  open FParsec

  let splitToTrimmedLines (input: string) =
    input.Trim().Split("\n")
    |> Seq.where (fun s -> s.Length > 0)
    |> Seq.map (fun s -> s.Trim())

  let splitToTrimmedLinesVerbose (input:string) =
    printfn "input: %A" input
    printfn "input.trimmed: %A" (input.Trim())
    printfn "input.trimmed.split: %A" (input.Trim().Split("\n"))
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())

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
