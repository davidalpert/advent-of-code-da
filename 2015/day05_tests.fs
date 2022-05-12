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

  [<Theory>]
  [<InlineData("xyxy", true)>]
  [<InlineData("aabcdefgaa", true)>]
  [<InlineData("aaa", false)>]
  [<InlineData("xxyxx", true)>]
  [<InlineData("ieodomkazucvgmuy", false)>]
  let ``Day 5 - part 2 - containsPairOfLetters`` (input:string, expected:bool) =
    input
    // |> firstCoinSuffix lengthOfPrefix
    |> containsPairOfLetters2
    |> should equal expected

  [<Theory>]
  [<InlineData("xyx", true)>]
  [<InlineData("abcdefeghi", true)>]
  [<InlineData("aaa", true)>]
  [<InlineData("xxyxx", true)>]
  [<InlineData("ieodomkazucvgmuy", true)>]
  let ``Day 5 - part 2 - atLeastOneRepeatedLetter`` (input:string, expected:bool) =
    input
    // |> firstCoinSuffix lengthOfPrefix
    |> atLeastOneRepeatedLetter
    |> should equal expected

  [<Theory>]
  [<InlineData("qjhvhtzxzqqjkmpb", true)>] // because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz).
  [<InlineData("xxyxx", true)>] // because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap.
  [<InlineData("uurcxstgmygtbstg", false)>] // because it has a pair (tg) but no repeat with a single letter between them.
  [<InlineData("ieodomkazucvgmuy", false)>] // because it has a repeating letter with one between (odo), but no pair that appears twice.
  let ``Day 5 - part 2 - isNice2`` (input:string, expectedNice:bool) =
    input
    |> isNice2
    |> should equal expectedNice

  [<Fact>]
  let ``Day 5 - part 2 - calculation`` () =
    day05input
    |> splitToTrimmedLines
    // |> firstCoinSuffix lengthOfPrefix
    |> Seq.map isNice2
    |> Seq.filter (fun nice -> nice)
    |> Seq.length
    |> should equal 55
