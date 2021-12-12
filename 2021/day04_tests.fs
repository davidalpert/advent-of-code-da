namespace AdventOfCode.Tests

module Day04 =

  open Xunit
  open FsUnit.Xunit
  open AdventOfCode.Input
  open AdventOfCode.Bingo

  [<Fact>]
  let ``Day 04 - Part 1 - Bingo Cards - Empty`` () =
    let card =
      [
        Unmarked(1);  Unmarked(2);  Unmarked(3);  Unmarked(4);  Unmarked(5);
        Unmarked(6);  Unmarked(7);  Unmarked(8);  Unmarked(9);  Unmarked(10);
        Unmarked(11); Unmarked(12); Unmarked(13); Unmarked(14); Unmarked(15);
        Unmarked(16); Unmarked(17); Unmarked(18); Unmarked(19); Unmarked(20);
        Unmarked(21); Unmarked(22); Unmarked(19); Unmarked(24); Unmarked(25);
      ]
      |> Card

    card.hasWon
    |> should equal false

  [<Fact>]
  let ``Day 04 - Part 1 - Bingo Cards - Row`` () =
    let card =
      [
        Unmarked(1);  Unmarked(2);  Unmarked(3);  Unmarked(4);  Unmarked(5);
        Marked;       Marked;       Marked;       Marked;       Marked;
        Unmarked(11); Unmarked(12); Unmarked(13); Unmarked(14); Unmarked(15);
        Unmarked(16); Unmarked(17); Marked;       Unmarked(19); Unmarked(20);
        Unmarked(21); Unmarked(22); Unmarked(19); Unmarked(24); Unmarked(25);
      ]
      |> Card

    card.hasWon
    |> should equal true

  [<Fact>]
  let ``Day 04 - Part 1 - Bingo Cards - Col`` () =
    let card =
      [
        Unmarked(1);  Unmarked(2);  Unmarked(3);  Marked; Unmarked(5);
        Unmarked(6);  Marked;       Unmarked(8);  Marked; Unmarked(10);
        Unmarked(11); Unmarked(12); Unmarked(13); Marked; Unmarked(15);
        Unmarked(16); Unmarked(17); Unmarked(18); Marked; Unmarked(20);
        Unmarked(21); Unmarked(22); Unmarked(19); Marked; Unmarked(25);
      ]
      |> Card

    card.hasWon
    |> should equal true

  [<Fact>]
  let ``Day 04 - Part 1 - Bingo Cards - Diagonal`` () =
    let card =
      [
        Marked;       Unmarked(2);  Unmarked(3);  Unmarked(4);  Unmarked(5);
        Unmarked(6);  Marked;       Unmarked(8);  Unmarked(9);  Unmarked(10);
        Unmarked(11); Unmarked(12); Marked;       Unmarked(14); Unmarked(15);
        Unmarked(16); Unmarked(17); Unmarked(18); Marked;       Unmarked(20);
        Unmarked(21); Unmarked(22); Unmarked(19); Unmarked(24); Marked; 
      ]
      |> Card

    card.hasWon
    |> should equal true

  [<Fact>]
  let ``Day 04 - Part 1 - Bingo Cards - Can Play`` () =
    let card =
      [
        Unmarked(1);  Unmarked(2);  Unmarked(3);  Unmarked(4);  Unmarked(5);
        Marked;       Marked;       Marked;       Marked;       Unmarked(10);
        Unmarked(11); Unmarked(12); Unmarked(13); Unmarked(14); Unmarked(15);
        Unmarked(16); Unmarked(17); Marked;      Unmarked(19); Unmarked(20);
        Unmarked(21); Unmarked(22); Unmarked(19); Unmarked(24); Unmarked(25);
      ]
      |> Card

    card.play(5).hasWon
    |> should equal false

    card.play(5).play(10).hasWon
    |> should equal true

  [<Fact>]
  let ``Day 04 - Part 1 - Bingo Cards - Can Score`` () =
    let game =
      [
        [
          22;13;17;11; 0;
           8; 2;23; 4;24;
          21; 9;14;16; 7;
           6;10; 3;18; 5;
           1;12;20;15;19;

        ] |> Card;
        [
           3;15; 0; 2;22;
           9;18;13;17; 5;
          19; 8; 7;25;23;
          20;11;10;24; 4;
          14;21;16;12; 6;
        ] |> Card;
        [
          14;21;17;24; 4;
          10;16;15; 9;19;
          18; 8;23;26;20;
          22;11;13; 6; 5;
           2; 0;12; 3; 7;
        ] |> Card;
      ]
      |> Game

    let playNumber (game:Game) (n:int) =
      game.play(n)

    let firstFive = [7; 4; 9; 5; 11]
    let afterFirstFive =
      firstFive
      |> List.fold playNumber game 
    
    afterFirstFive.hasWinner
    |> should equal false

    let nextSix = [17; 23; 2; 0; 14; 21]
    let afterNextSix =
      nextSix
      |> List.append nextSix
      |> List.fold playNumber afterFirstFive 

    afterNextSix.hasWinner
    |> should equal false

    let lastDraw = 24
    let endState =
      afterNextSix.play(lastDraw)

    // endState.printf

    endState.hasWinner
    |> should equal true

    let card = endState.winningCard
    
    card.unmarkedValue
    |> should equal 188

    card.score
    |> should equal 4512

  [<Fact>]
  let ``Day 04 - Part 1 - Sample`` () =
    day04sample
    |> calculateWinningScore
    |> should equal 4512

  [<Fact>]
  let ``Day 04 - Part 1 - Data`` () =
    day04data
    |> calculateWinningScore
    |> should equal 34506

  [<Fact>]
  let ``Day 04 - Part 2 - Sample`` () =
    printfn "Day 04 - Part 2 - Sample"
    day04sample
    |> calculateLosingScore false
    |> should equal 1924

  [<Fact>]
  let ``Day 04 - Part 2 - Data`` () =
    printfn "Day 04 - Part 2 - Data"
    day04data
    |> calculateLosingScore false
    |> should equal 7686

  [<Fact>]
  let ``Day 04 - Part 1 - Ray's Data`` () =
    day04raysdata
    |> calculateWinningScore
    |> should equal 72770

  [<Fact>]
  let ``Day 04 - Part 2 - Ray's Data`` () =
    day04raysdata
    |> calculateLosingScore false
    |> should equal 13912