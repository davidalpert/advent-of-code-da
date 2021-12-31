namespace AdventOfCode

module SnailfishMath =

  // Snailfish numbers aren't like regular numbers. Instead, every snailfish number is a pair - an ordered list of two elements. Each element of the pair can be either a regular number or another pair.
  type Pair = 
  | Value of int 
  | Pair of Pair * Pair

  with

    // Pairs are written as [x,y], where x and y are the elements within the pair. Here are some example snailfish numbers, one snailfish number per line:
    member pair.toString =
      match pair with
      | Value(v) -> sprintf "%d" v
      | Pair(left,right) -> sprintf "[%s,%s]" left.toString right.toString

    member pair.explodes (depth:int) =
      depth > 4

    // To split a regular number, replace it with a pair; the left element of the pair should be the regular number divided by two and rounded down, while the right element of the pair should be the regular number divided by two and rounded up. For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.
    member pair.split =
      match pair with

      | Value(v) ->
        if v < 9 then
          (false, pair)
        else
          let left = v / 2
          let right = v - left
          (true, Pair(left |> Value, right |> Value))

      | Pair(left,right) ->
        let leftHasSplit, leftSplit = left.split
        if leftHasSplit then
          (true, Pair(leftSplit, right))
        else
          let rightHasSplit,rightSplit = right.split
          (rightHasSplit, Pair(left,rightSplit))

    member pair.reduce (depth:int) =
      let innerDepth = depth + 1 
      match pair with
      | Value(v) ->
        if v < 9 then
          Value(v)
        else
          // split

          Value(v)

      | _ -> failwith "TBD"
          // split
    static member (+) (left:Pair, right:Pair) = Pair(left,right).reduce 0


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
