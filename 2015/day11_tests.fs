namespace AdventOfCode.Tests

module Day11 =

    open AdventOfCode.Input
    open AdventOfCode.CorporatePolicy
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData('a', 'b')>]
    [<InlineData('b', 'c')>]
    [<InlineData('y', 'z')>]
    [<InlineData('z', 'a')>]
    let ``Day 11 - part 1 - nextChar`` (c, expected) = c |> nextChar |> should equal expected

    [<Theory>]
    [<InlineData("xxxxxxxx", "xxxxxxxy")>]
    [<InlineData("xxxxxxxy", "xxxxxxxz")>]
    [<InlineData("xxxxxxyz", "xxxxxxza")>]
    [<InlineData("xxxxxxya", "xxxxxxyb")>]
    [<InlineData("zzzzzzzz", "aaaaaaaa")>]
    let ``Day 11 - part 1 - nextPassword`` (s, expected) =
        s |> nextPassword |> should equal expected

    [<Theory>]
    [<InlineData("hijklmmn", true)>]
    [<InlineData("abbceffg", false)>]
    let ``Day 11 - part 1 - includesIncreasingStraightOfMinNChars`` (s, expected) =
        s
        |> Array.ofSeq
        |> includesIncreasingStraightOfMinNChars 3
        |> should equal expected

    [<Theory>]
    [<InlineData("hijklmmn", "iol", false)>]
    [<InlineData("ghijklmn", "iol", false)>]
    [<InlineData("abbceffg", "iol", true)>]
    let ``Day 11 - part 1 - doesNotContainChars`` (s, ss, expected) =
        s
        |> Array.ofSeq
        |> doesNotContainChars ss
        |> should equal expected

    [<Theory>]
    [<InlineData("abbceffg", true)>]
    [<InlineData("abbcegjk", false)>]
    [<InlineData("abbbcefg", false)>]
    let ``Day 11 - part 1 - containsAtLeastTwoDifferentNonOverlappingPairs`` (s, expected) =
        s
        |> Array.ofSeq
        |> containsAtLeastTwoDifferentNonOverlappingPairs
        |> should equal expected

// [<Theory>]
// [<InlineData("1", 1, "11")>]
// [<InlineData("1", 2, "21")>]
// [<InlineData("1", 3, "1211")>]
// [<InlineData("1", 4, "111221")>]
// [<InlineData("1", 5, "312211")>]
// let ``Day 10 - part 1 - lookAndSayNTimes`` (s, n, expected) =
//   s
//   |> (lookAndSayNTimes n false)
//   |> should equal expected

// [<Fact>]
// let ``Day 10 - part 1 - calculation`` () =
//   day10input
//   |> (lookAndSayNTimes 40 false)
//   |> (fun s -> s.Length)
//   |> should equal 329356

// [<Fact>]
// let ``Day 10 - part 2 - calculation`` () =
//   day10input
//   |> (lookAndSayNTimes 50 false)
//   |> (fun s -> s.Length)
//   |> should equal 4666278
