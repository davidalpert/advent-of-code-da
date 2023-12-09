namespace AdventOfCode

open System
open System.Threading.Tasks

module DummyModuleOnWhichToAttachAssemblyAttribute =
    open ApprovalTests.Reporters;

    [<assembly: UseReporter(typeof<DiffReporter>)>]
    do ()
    
// https://fssnip.net/pa
module ArrayP =
  let reduceParallel<'a> f (ie :'a array) =
    let rec reduceRec f (ie :'a array) = function
      | 1 -> ie.[0]
      | 2 -> f ie.[0] ie.[1]
      | len ->
        let h = len / 2
        let o1 = Task.Run(fun _ -> reduceRec f (ie |> Array.take h) h)
        let o2 = Task.Run(fun _ -> reduceRec f (ie |> Array.skip h) (len-h))
        Task.WaitAll(o1, o2)
        f o1.Result o2.Result
    match ie.Length with
    | 0 -> failwith "Sequence contains no elements"
    | c -> reduceRec f ie c

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

  // https://gist.github.com/krishnabhargav/da6686e295638d000aab
  let rec gcd a b =
    match (a,b) with
    | (x,y) when x = y -> x
    | (x,y) when x > y -> gcd (x-y) y
    | (x,y)            -> gcd x (y-x)

  // https://gist.github.com/krishnabhargav/da6686e295638d000aab
  let lcm a b = a*b/(gcd a b)

  // https://stackoverflow.com/a/7267104/8997
  module Prime =
    let isPrime n = 
        let bound = int (sqrt(float n))
        seq{2..bound}
        |> Seq.exists (fun x -> n % x = 0) 
        |> not
    let rec nextPrime n = 
        if isPrime (n + 1) then n + 1
        else nextPrime (n+1)
    let sequence = 
        Seq.unfold(fun n -> Some(n, nextPrime n)) 1

  // https://blog.mikaellundin.name/2011/08/13/infinite-sequence-of-primes-by-sequence-expressions.html
  module Prime2 =
    let rec primes =
      let isPrime number primes =
        let sqrtn = float >> sqrt >> int
        primes
         |> Seq.takeWhile (fun n -> n <= (sqrtn number))
         |> Seq.exists (fun n -> number % n = 0)
       |> not

      let rec primes' current =
        seq {
         if primes |> isPrime current then
           yield current
           yield! primes' (current + 2)
        }
      seq {
        yield 2
        yield! primes' 3
      } |> Seq.cache

  // folding the recursive nextPrime into a where filter
  // against the infinite sequence of positive integers
  let allPrimes =
      Seq.unfold (fun n -> Some(n, n+1)) 1
      |> Seq.where Prime.isPrime
  
  let primeFactorsOf n =
      allPrimes
      |> Seq.takeWhile (fun p -> p <= n)
      |> Seq.where (fun p -> n % p = 0)

  // https://www.calculatorsoup.com/calculators/math/lcm.php
  let lcmByVennDiagram(aa:int seq) =
    // printfn "lcmByVennDiagram: %A" aa
    aa
    |> Seq.map (fun a -> Set(primeFactorsOf a))
    // |> Seq.map (fun a ->
    //     printfn $"- %A{a}"
    //     a
    //   )
    |> Set.unionMany
    |> Seq.map int64 // make sure we don't overflow while reducing
    |> Seq.reduce (*)