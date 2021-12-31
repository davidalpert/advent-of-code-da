namespace AdventOfCode

module SnailfishMath =

  // Snailfish numbers aren't like regular numbers. Instead, every snailfish number is a pair - an ordered list of two elements. Each element of the pair can be either a regular number or another pair.
  type Pair = 
  | Value of int 
  | Pair of Pair * Pair

  with

    // Pairs are written as [x,y], where x and y are the elements within the pair. Here are some example snailfish numbers, one snailfish number per line:
    member x.toString =
      match x with
      | Value(v) -> sprintf "%d" v
      | Pair(x,y) -> sprintf "[%s,%s]" x.toString y.toString

    static member (+) (left:Pair, right:Pair) = Pair(left,right)


module SnailfishMathParser =
  open FParsec
  open FParsec.Pipes
  open SnailfishMath

  let value = 
    %% +.p<int> -%> Value

  let pair, pairRef = createParserForwardedToRef<Pair,unit>()

  let compound =
    %% '[' ?- +.pair-- ',' -- +.pair -- ']'
    -|> fun x y -> Pair(x,y)

  do pairRef.Value <- choice [value; compound]

  let parse (input:string) =
    match run pair input with
    | Success(r, _, _) -> Result.Ok(r)
    | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

  let mustParse (input:string) =
    match run pair input with
    | Success(r, _, _) -> r
    | Failure(errorMsg, _, _) -> failwith errorMsg
