namespace AdventOfCode.Tests

module Day12 =

    open AdventOfCode.Input
    open AdventOfCode.JsonAbacus
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData("[1,2,3]", 6)>]
    [<InlineData("""{"a":2,"b":4}""", 6)>]
    [<InlineData("""[[[3]]]""", 3)>]
    [<InlineData("""{"a":{"b":4},"c":-1}""", 3)>]
    [<InlineData("""{"a":[-1,1]}""", 0)>]
    [<InlineData("""[-1,{"a":1}]""", 0)>]
    [<InlineData("""[]""", 0)>]
    [<InlineData("""{}""", 0)>]
    let ``Day 12 - part 1 - examples`` (s, expected) =
        s
        |> sumOfAllNumbers
        |> should equal (expected |> float)

    [<Fact>]
    let ``Day 12 - part 1 - calculation`` () =
        day12input
        |> sumOfAllNumbers
        |> should equal (191164 |> float)

    [<Theory>]
    [<InlineData("[1,2,3]", 6)>]
    [<InlineData("""{"a":2,"b":4}""", 6)>]
    [<InlineData("""[[[3]]]""", 3)>]
    [<InlineData("""{"a":{"b":4},"c":-1}""", 3)>]
    [<InlineData("""{"a":[-1,1]}""", 0)>]
    [<InlineData("""[-1,{"a":1}]""", 0)>]
    [<InlineData("""[]""", 0)>]
    [<InlineData("""{}""", 0)>]
    [<InlineData("""[1,{"c":"red","b":2},3]""", 4)>]
    [<InlineData("""{"d":"red","e":[1,2,3,4],"f":5}""", 0)>]
    [<InlineData("""[1,"red",5]""", 6)>]
    let ``Day 12 - part 2 - examples`` (s, expected) =
        s
        |> sumOfAllNumbers2
        |> should equal (expected |> float)

    [<Fact>]
    let ``Day 12 - part 2 - calculation`` () =
        day12input
        |> sumOfAllNumbers2
        |> should equal (87842 |> float)
