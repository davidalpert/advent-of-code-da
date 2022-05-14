namespace AdventOfCode.Tests

module Day10 =

  open AdventOfCode.Input
  open AdventOfCode.LookAndSay
  open System.Collections.Generic
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData("1", "11")>]
  [<InlineData("2", "12")>]
  [<InlineData("11", "21")>]
  [<InlineData("21", "1211")>]
  [<InlineData("1211", "111221")>]
  [<InlineData("111221", "312211")>]
  let ``Day 10 - part 1 - examples`` (s, expected) =
    s
    |> lookAndSay
    |> should equal expected

  [<Theory>]
  [<InlineData("1", 1, "11")>]
  [<InlineData("1", 2, "21")>]
  [<InlineData("1", 3, "1211")>]
  [<InlineData("1", 4, "111221")>]
  [<InlineData("1", 5, "312211")>]
  let ``Day 10 - part 1 - lookAndSayNTimes`` (s, n, expected) =
    s
    |> (lookAndSayNTimes n false)
    |> should equal expected

  [<Fact>]
  let ``Day 10 - part 1 - calculation`` () =
    day10input
    |> (lookAndSayNTimes 40 false)
    |> (fun s -> s.Length)
    |> should equal 329356

  [<Fact>]
  let ``Day 10 - part 2 - calculation`` () =
    day10input
    |> (lookAndSayNTimes 50 false)
    |> (fun s -> s.Length)
    |> should equal 4666278