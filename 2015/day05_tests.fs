namespace AdventOfCode.Tests

module Day05 =

  open AdventOfCode.Input
  open AdventOfCode.NaughtyDetector
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData("ugknbfddgicrmopn ", true)>] // because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
  [<InlineData("aaa ", true)>] // because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
  [<InlineData("jchzalrnumimnmhp ", false)>] // because it has no double letter.
  [<InlineData("haegwjzuvuyypxyu", false)>] // because it contains the string xy.
  [<InlineData("dvszwmarrgswjxmb", false)>] // because it contains only one vowel.

  let ``Day 5 - part 1 - tests`` (input:string, expectedNice:bool) =
    input
    // |> firstCoinSuffix lengthOfPrefix
    |> isNice
    |> should equal expectedNice

  [<Fact>]
  let ``Day 5 - part 1 - calculation`` () =
    day05input
    |> splitToTrimmedLines
    // |> firstCoinSuffix lengthOfPrefix
    |> Seq.map isNice
    |> Seq.filter (fun nice -> nice)
    |> Seq.length
    |> should equal 255

