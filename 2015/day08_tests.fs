namespace AdventOfCode.Tests

module Day08 =

  open AdventOfCode.Input
  open AdventOfCode.Matchsticks
  open System.Collections.Generic
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData("\"", 1)>]
  [<InlineData("\\\\", 2)>]
  [<InlineData("\\\"", 2)>]
  let ``d``(s:string, expectedLength:int) =
    s.Length |> should equal expectedLength

  [<Theory>]
  [<InlineData("""
""
""", 2, 0)>]
  [<InlineData("""
"abc"
""", 5, 3)>]
  [<InlineData("""
"aaa\"aaa"
""", 10, 7)>]
  [<InlineData("""
"\x27"
""", 6, 1)>]
  let ``Day 8 - part 1 - tests - parseRawString`` (s:string, nCharsStringLiterals:int, nLengthInMemory:int) =
    s
    |> splitToTrimmedLines
    |> Seq.head
    |> parseRawString
    |> should equal (nCharsStringLiterals, nLengthInMemory)

  [<Theory>]
  [<InlineData("""
""
"abc"
"aaa\"aaa"
"\x27"
""", 23, 11, 12)>]
  let ``Day 8 - part 1 - tests - calculateDifference`` (s:string, rawLength:int, lengthInMemory:int, expected:int) =
    s
    |> splitToTrimmedLines
    |> calculateDifference
    |> should equal expected

  [<Fact>]
  let ``Day 8 - part 1 - calculation`` () =
    day08input
    |> splitToTrimmedLines
    |> calculateDifference
    |> should equal 1342

  [<Theory>]
  [<InlineData("\"\"", "\\\"\\\"")>]
  [<InlineData("\"abc\"", "\\\"abc\\\"")>]
  [<InlineData("\"aaa\"aaa\"", "\\\"aaa\\\"aaa\\\"")>]
  [<InlineData("\"\\x27\"", "\\\"\\\\x27\\\"")>]
  let ``Day 8 - part 1 - tests - encodeRawString`` (s:string, expected:string) =
    s
    |> encodeRawString
    |> should equal expected

  [<Theory>]
  [<InlineData("""
""
"abc"
"aaa\"aaa"
"\x27"
""", 42, 23, 19)>]
  let ``Day 8 - part 2 - tests - calculateDifference2`` (s:string, encodedLength:int, rawLength:int, expected:int) =
    s
    |> splitToTrimmedLines
    |> calculateDifference2
    |> should equal expected

  [<Fact>]
  let ``Day 8 - part 2 - calculation`` () =
    day08input
    |> splitToTrimmedLines
    |> calculateDifference2
    |> should equal 2074
