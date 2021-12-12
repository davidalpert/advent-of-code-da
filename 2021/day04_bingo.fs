namespace AdventOfCode

module Bingo =

  open System
  open AdventOfCode.Input
  open AdventOfCode.utils

  type squareState =
  | Unmarked of int
  | Marked

  type Card(lastNumberPlayed:int, squares:squareState list) =
    let state = squares

    with
      // new from square values
      new(numbers:int list) =
        let squares =
          numbers
          |> List.ofSeq
          |> List.map Unmarked
        Card(-1,squares)

      // new from squares
      new(squares:squareState list) =
        Card(-1,squares)

      member x.play (n:int) =
        state
        |> List.map (fun s ->
            match s with
            | Marked      -> Marked
            | Unmarked(v) -> if v = n then Marked else Unmarked(v)
          )
        |> Card.applyPlay n

      static member applyPlay (numberPlayed:int) (squares: squareState list) =
        Card(numberPlayed, squares)

      member x.unmarkedValue =
        let accumulatedUnmarkedSqareValues (currentScore:int) (square:squareState) =
          match square with
          | Marked          -> currentScore
          | Unmarked(value) -> currentScore + value

        state
        |> List.fold accumulatedUnmarkedSqareValues 0

      member x.score =
        if lastNumberPlayed < 0 then
          0
        else
          x.unmarkedValue * lastNumberPlayed

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
        |> List.iter (fun row -> 
          let stringValueFor (s:squareState) =
            match s with
            | Marked      -> " X"
            | Unmarked(n) -> sprintf "%2i" n
          printfn "%A" (row |> List.map stringValueFor)
        )

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

      member x.cardsWhichHaveNotYetWon =
          state
          |> List.filter (fun card -> card.hasWon = false)

  let inputToBallsAndInitialGameState (input:string) =
    let lines =
      input
      |> splitToTrimmedLines
      |> List.ofSeq

    // first line is set of balls/numbers called
    let balls =
      lines.Head.Split ','
      |> List.ofArray
      |> List.map int

    // remaining lines are groups of 6 (1 + Card.dimension)
    // each starts with a blank line and then 5 lines of numbers
    let cards =
      lines.Tail
      |> List.chunkBySize (1 + Card.dimemsions)
      // first line is empty
      |> List.map (fun ll -> ll.Tail)
      // each list of lines represents a Card
      |> List.map (fun cardLines ->
          cardLines
          |> List.map (fun line ->
            line.Split " "
            |> List.ofArray
            |> List.filter (fun x -> x <> String.Empty)
          )
          |> List.concat
          |> List.map int
          |> Card
        )
    
    let game =
      cards
      |> Game

    (balls,game)

  let calculateWinningScore (input:string) =
    let (balls,game) = input |> inputToBallsAndInitialGameState

    let playUntilWinner (game:Game) (n:int) =
      if game.hasWinner then
        game
      else
        game.play(n)

    let finishedGame =
      balls
      |> List.fold playUntilWinner game 

    if finishedGame.hasWinner then
      finishedGame.winningCard.score
    else
      0

  let calculateLosingScore (input:string) =
    let (balls,game) = input |> inputToBallsAndInitialGameState

    let playUntilOneGameLeft (game:Game,losingCard:Card option,lastPlay:int) (thisPlay:int) =
      let remainingCards = game.cardsWhichHaveNotYetWon
      match (remainingCards,losingCard) with
      // we already have the losing card; preserve the previous next play
      | (_ :: [],Some(loser)) -> (game,Some(loser),lastPlay)
      // we just found the losing card; save the current next play
      | (loser :: [],None)    -> (game,Some(loser),thisPlay)
      // we have multiple losers; keep playing
      | _                     -> (game.play(thisPlay),None,thisPlay)

    let playUntilOneGameLeftWatched  (game:Game,losingCard:Card option,lastPlay:int) (thisPlay:int)  =
      let (game2,card2,lastPlayed2) = playUntilOneGameLeft (game, losingCard, lastPlay) thisPlay

      printfn "after playing: %d" thisPlay
      game2.printf

      (game2,card2,lastPlayed2)

    let (nearlyFinishedGame,losingCard,lastNumberToPlay) =
      balls
      |> List.fold playUntilOneGameLeft (game,None,-1) 

    printfn "nearly finished:"
    losingCard.Value.printf
    printfn "about to play: %d" lastNumberToPlay

    losingCard.Value.play(lastNumberToPlay).score
    