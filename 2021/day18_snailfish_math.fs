namespace AdventOfCode

module SnailfishMath =

  // debug helpers
  let ( ~~~ ) =
    // printfn
    sprintf

  let ( ~~~~ ) =
    // printfn
    sprintf

  let ( ~~~~~ ) =
    // printfn
    sprintf


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
        if v > 9 then
          let left = v / 2
          let right = v - left
          (true, Pair(left |> Value, right |> Value))
        else
          (false, pair)

      | Pair(left,right) ->
        let leftHasSplit, leftSplit = left.split
        if leftHasSplit then
          (true, Pair(leftSplit, right))
        else
          let rightHasSplit,rightSplit = right.split
          (rightHasSplit, Pair(left,rightSplit))

    member private pair.applyExplosion (explodingLeft:int option) (explodingRight:int option) =
      ~~~~ "%s: applying explosion %A <--|--> %A" pair.toString explodingLeft explodingRight

      match pair with
      | Value(v) -> false, pair // don't apply an explosion to a value because we don't know which direction we are exploding
      | Pair(left,right) ->
        if explodingLeft.IsSome then
          match right with
          | Value(v)  -> true, Pair(left, Value(v + explodingLeft.Value))
          | Pair(_,_) ->
            let applied, updatedRight = right.applyExplosion explodingLeft explodingRight
            applied, Pair(left, updatedRight)
        else // explodingRight
          match left with
          | Value(v)  -> true, Pair(Value(v + explodingRight.Value), right)
          | Pair(_,_) ->
            let applied, updatedLeft = left.applyExplosion explodingLeft explodingRight
            applied, Pair(updatedLeft, right)

    // To explode a pair, the pair's left value is added to the first regular number to the left of the exploding pair (if any), and the pair's right value is added to the first regular number to the right of the exploding pair (if any). Exploding pairs will always consist of two regular numbers. Then, the entire exploding pair is replaced with the regular number 0.
    member private pair.explodeAtDepth (depth:int) : Option<int * int> * Pair =
      ~~~~~ "%s: exploding at depth %d" pair.toString depth

      let isValue (p:Pair) =
        match p with
        | Value(_) -> true
        | Pair(_,_) -> false

      let asValue (p:Pair) =
        match p with
        | Value(v) -> v
        | Pair(_,_) -> failwith "Pair is not a Value"

      match pair with
      | Value(v) -> None, pair // a simple Value doesn't explode

      | Pair(left,right) ->
     
        // an inner pair of Values deeper than 4 explodes (inside-out)
        if depth > 4 && (left |> isValue) && (right |> isValue) then
          let explodingLeft = left |> asValue
          let explodingRight = right |> asValue
          ~~~~ "pair explodes: %A <--|--> %A" explodingLeft explodingRight
          Some(explodingLeft, explodingRight), Value(0)

        else

          // check left for explosions
          match left.explodeAtDepth (depth+1) with

          | Some(explodingLeft, explodingRight), newLeft ->
            ~~~~~ "%s: left exploded %A <--|--> %A" pair.toString explodingLeft explodingRight

            // merge right
            match right with
            | Value(v) ->  Some(explodingLeft, 0), Pair(newLeft, Value(explodingRight + v))
            | Pair(_,_) ->
              let applied, updatedRight = right.applyExplosion None (Some explodingRight)
              if applied then
                Some(explodingLeft, 0), Pair(newLeft, updatedRight)
              else
                Some(explodingLeft, explodingRight), Pair(newLeft, updatedRight)

          // left did not explode
          | None, _ -> match right.explodeAtDepth (depth + 1) with

                       | Some(explodingLeft, explodingRight), newRight ->
                          ~~~~~ "%s: right exploded %A <--|--> %A" pair.toString explodingLeft explodingRight

                          // merge left
                          match left with
                          | Value(v) -> Some(0, explodingRight), Pair(Value(v + explodingLeft), newRight)
                          | Pair(_,_) ->
                            let applied, updatedLeft = left.applyExplosion (Some explodingLeft) None
                            if applied then 
                              Some(0, explodingRight), Pair(updatedLeft, newRight)
                            else
                              Some(explodingLeft, explodingRight), Pair(updatedLeft, newRight)

                       // neither child exploded; as you were...
                       | None, _ -> None, pair

    member pair.explode =
      match pair.explodeAtDepth 1 with
      | Some(_,_),p -> true, p
      | None, p     -> false, p

    member pair.reduce =
      match pair with
      | Value(v) -> pair
      | Pair(_,_) ->
        let mutable reducing = true
        let mutable currentPair = pair

        while reducing do
          // ~~~ "%s : reducing" currentPair.toString
          // try to explode
          let exploded, resultingPair = currentPair.explode
          if exploded then
            ~~~ "%s : exploded" currentPair.toString
            currentPair <- resultingPair
          else
            // try to split
            let split, splitPair = currentPair.split
            if split then
              ~~~ "%s : split" currentPair.toString
              currentPair <- splitPair
            else
              ~~~ "%s : done" currentPair.toString
              reducing <- false

        currentPair

    static member (+) (left:Pair, right:Pair) = Pair(left,right).reduce

    static member (++) (left:Pair, right:Pair) = // + with debugging
      let result = Pair(left,right)
      ~~~ "---------\nadding:\n%s\n%s\n%s\n" left.toString right.toString result.toString
      Pair(left,right).reduce

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
