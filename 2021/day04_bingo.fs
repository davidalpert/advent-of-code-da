namespace AdventOfCode

module Bingo =

  open AdventOfCode.utils

  type squareState =
  | Unmarked of int
  | Marked

  type Card(squares:squareState list) =
    let state = squares

    with
      new(numbers:int list) =
        let squares =
          numbers
          |> List.ofSeq
          |> List.map Unmarked
        Card(squares)

      member x.play (n:int) =
        state
        |> List.map (fun s ->
            match s with
            | Marked      -> Marked
            | Unmarked(v) -> if v = n then Marked else Unmarked(v)
          )
        |> Card

      // static member applyPlay (play:int) (squares: squareState list) =
      //   Card(play, squares)

      member x.unmarkedValue =
        let accumulatedUnmarkedSqareValues (currentScore:int) (square:squareState) =
          match square with
          | Marked          -> currentScore
          | Unmarked(value) -> currentScore + value

        state
        |> List.fold accumulatedUnmarkedSqareValues 0

      member x.hasWon =
        let winningLine (squares:squareState list) =
          let numberMarked =
            squares
            |> List.filter (fun s ->
                match s with
                | Marked -> true
                | _ -> false
              )
            |> List.length

          numberMarked = Card.dimemsions

        let hasWinningRow =
          state
          |> List.chunkBySize Card.dimemsions
          |> List.exists winningLine
        
        let hasWinningCol =
          state
          |> List.chunkBySize Card.dimemsions
          |> transpose
          |> List.exists winningLine

        let hasWinningDiagonal =
          seq { 1 .. (Card.dimemsions) }
          |> Seq.map (fun n ->
                ((state |> List.chunkBySize Card.dimemsions |> Array.ofList).[n - 1] |> Array.ofList).[n - 1]
             )
          |> List.ofSeq
          |> winningLine

        hasWinningRow || hasWinningCol || hasWinningDiagonal

      static member dimemsions = 5

      static member tryCreateCard(squares:int list) =
        let numberOfSquaresGiven = squares |> Seq.length
        if numberOfSquaresGiven = (Card.dimemsions * Card.dimemsions) then
          Some(Card(squares))
        else
          None

      member x.printf =
        state
        |> List.chunkBySize Card.dimemsions
        |> List.iter (fun row -> printfn "%A" row)

  type Game(cards:Card list) =
    let state = cards

    with
      member x.printf =
        state
        |> List.iter (fun g ->
          printfn "------"
          g.printf
        )

      member x.play (n:int) =
        state
        |> List.map (fun c -> c.play n)
        |> Game

      member x.winningCard =
        state
        |> List.find (fun c -> c.hasWon)

      member x.tryFindWinner =
        state
        |> List.tryFind (fun c -> c.hasWon)

      member x.hasWinner =
        match x.tryFindWinner with
        | Some _ -> true
        | None   -> false

    // let play n =
