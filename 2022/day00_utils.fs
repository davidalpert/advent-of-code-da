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

  let splitBy (sep: string) (input: string) =
    input.Split(sep)
        
  let joinBy (sep:string) (values:string[]) =
     System.String.Join(sep, values)
     
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
