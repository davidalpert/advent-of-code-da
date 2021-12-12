namespace AdventOfCode.Tests

module Day04 =

  open Xunit
  open FsUnit.Xunit
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