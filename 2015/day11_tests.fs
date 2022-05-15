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
    [<InlineData("abcdegaa", "abcdegab")>]
    [<InlineData("abcdeaaz", "abcdeaba")>]
    [<InlineData("abadzzzz", "abaeaaaa")>]
    let ``Day 11 - part 1 - nextPassword`` (s, expected) =
        s |> nextPassword |> should equal expected

    [<Theory>]
    [<InlineData("hijklmmn", true)>]
    [<InlineData("abbceffg", false)>]
    let ``Day 11 - part 1 - includesIncreasingStraightOfThreeChars`` (s, expected) =
        s
        |> Array.ofSeq
        |> includesIncreasingStraightOfThreeChars
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

    [<Theory>]
    [<InlineData("abcdefgh", "abcdffaa")>]
    [<InlineData("ghijklmn", "ghjaabcc")>]
    let ``Day 11 - part 1 - nextValidPassword`` (s, expected) =
        s |> nextValidPassword |> should equal expected

    [<Fact>]
    let ``Day 11 - part 1 - calculation`` () =
        "vzbxkghb"
        |> nextValidPassword
        |> should equal "vzbxxyzz"
