namespace AdventOfCode

module Day20 =

  open Input
  open System
  open Xunit
  open FsUnit.Xunit
  open TrenchMap

  [<Fact>]
  let ``Day 20 - tests - parse sample`` () =

    day20sample |> Parser.mustParse

  [<Fact>]
  let ``Day 20 - tests - parse algorithm length`` () =

    let image = day20sample |> Parser.mustParse

    image.EnhancementAlgorithm.Length |> should equal 512

  [<Fact>]
  let ``Day 20 - tests - image dimensions`` () =

    let image = day20sample |> Parser.mustParse

    image.dimensions |> should equal (5,5)

  [<Fact>]
  let ``Day 20 - tests - render`` () =

    let image = day20sample |> Parser.mustParse

    $"\n{image.render}\n"
    |> should equal """
#..#.
#....
##..#
..#..
..###
"""

  [<Fact>]
  let ``Day 20 - tests - getSurroundingCoords`` () =

    let image = day20sample |> Parser.mustParse

    image.getSurroundingCoords 5 10
    |> Seq.sort
    |> Array.ofSeq
    |> should equal [|
      (4,9); (4,10); (4,11);
      (5,9); (5,10); (5,11);
      (6,9); (6,10); (6,11);
    |]

  [<Fact>]
  let ``Day 20 - tests - toBit`` () =

    '#' |> toBit |> should equal 1

    '.' |> toBit |> should equal 0

    (fun () -> 'a' |> toBit |> ignore)
    |> should throw typeof<Exception>

  [<Theory>]
  [<InlineData(2, 2, "...#...#.")>]
  [<InlineData(0, 0, "....#..#.")>]
  let ``Day 20 - tests - charStringFor`` (x, y, expectedString) =

    let image = day20sample |> Parser.mustParse

    image.charStringFor x y
    |> should equal expectedString

  [<Theory>]
  [<InlineData(2, 2, "000100010")>]
  [<InlineData(0, 0, "000010010")>]
  let ``Day 20 - tests - bitStringFor`` (x, y, expectedString) =

    let image = day20sample |> Parser.mustParse

    image.bitStringFor x y
    |> should equal expectedString

  [<Theory>]
  [<InlineData(2, 2, 34)>]
  [<InlineData(0, 0, 18)>]
  let ``Day 20 - tests - decimalFor`` (x, y, expected) =

    let image = day20sample |> Parser.mustParse

    image.decimalFor x y
    |> should equal expected

  [<Fact>]
  let ``Day 20 - tests - expand`` () =

    let image = day20sample |> Parser.mustParse

    let expanded = image.expandBy 2

    $"\n{expanded.render}\n"
    |> should equal """
.........
.........
..#..#...
..#......
..##..#..
....#....
....###..
.........
.........
"""

  [<Fact>]
  let ``Day 20 - tests - contract`` () =

    let image = day20sample |> Parser.mustParse

    let expanded = image.expandBy 2

    let contracted = expanded.contractBy 2

    contracted.render |> should equal (image.render)

  [<Fact>]
  let ``Day 20 - tests - enhance`` () =

    let image = day20sample |> Parser.mustParse

    let expanded = image.expandBy 5

    $"\n{expanded.render}\n" |> should equal """
...............
...............
...............
...............
...............
.....#..#......
.....#.........
.....##..#.....
.......#.......
.......###.....
...............
...............
...............
...............
...............
"""

    let ehnanced = expanded.enhance

    $"\n{ehnanced.render}\n" |> should equal """
...............
...............
...............
...............
.....##.##.....
....#..#.#.....
....##.#..#....
....####..#....
.....#..##.....
......##..#....
.......#.#.....
...............
...............
...............
...............
"""

  [<Fact>]
  let ``Day 20 - tests - enhance twice`` () =

    let image = day20sample |> Parser.mustParse

    let expanded = image.expandBy 5

    let ehnanced = expanded.enhance

    $"\n{ehnanced.render}\n" |> should equal """
...............
...............
...............
...............
.....##.##.....
....#..#.#.....
....##.#..#....
....####..#....
.....#..##.....
......##..#....
.......#.#.....
...............
...............
...............
...............
"""

    let enhancedMore = ehnanced.enhance

    $"\n{enhancedMore.render}\n" |> should equal """
...............
...............
...............
..........#....
....#..#.#.....
...#.#...###...
...#...##.#....
...#.....#.#...
....#.#####....
.....#.#####...
......##.##....
.......###.....
...............
...............
...............
"""

  [<Fact>]
  let ``Day 20 - tests - enhanceBy`` () =

    let image = day20sample |> Parser.mustParse

    let expanded = image.expandBy 5

    let ehnanced = expanded.enhance

    let enhancedMore = ehnanced.enhance

    let enhancedTwice = expanded.enhanceBy 2

    $"\n{enhancedTwice.render}\n" |> should equal $"\n{enhancedMore.render}\n"

  [<Fact>]
  let ``Day 20 - tests - numberOfLitPixels`` () =

    let image = day20sample |> Parser.mustParse

    let expanded = image.expandBy 5

    let enhancedTwice = expanded.enhanceBy 2

    enhancedTwice.numberOfLitPixels
    |> should equal 35

