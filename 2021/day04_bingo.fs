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

  type Game(cards:Card seq) =
    let x = cards

    // let play n =
