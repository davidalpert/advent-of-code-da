namespace AdventOfCode

module Day10 =

  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData("()")>]
  let ``Day 10 - tests - valid chunks`` (input:string) =
    let r = NavigationParser.parseChunk input

    r
    |> should equal 1