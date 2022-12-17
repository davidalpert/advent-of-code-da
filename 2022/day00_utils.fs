namespace AdventOfCode

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