namespace AdventOfCode

module Day18 =

  open Input
  open Xunit
  open FsUnit.Xunit
  open SnailfishMath
  open SnailfishMathParser

  [<Fact>]
  let ``Day 18 - tests - snailfish numbers are not like regular numbers`` () =
    let n = Pair(Value(1),Value(2))

    n.toString |> should equal "[1,2]"

  [<Theory>]
  [<InlineData("[1,2]")>]
  [<InlineData("[[1,2],3]")>]
  [<InlineData("[9,[8,7]]")>]
  [<InlineData("[[1,9],[8,5]]")>]
  [<InlineData("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]")>]
  [<InlineData("[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]")>]
  [<InlineData("[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")>]
  let ``Day 18 - tests - roundtrip parse / toString`` (input:string) =
    (mustParse input).toString |> should equal input

  [<Theory>]
  [<InlineData("[5,2]", "[5,2]")>]
  [<InlineData("[15,2]", "[[7,8],2]")>]
  [<InlineData("[0,13]", "[0,[6,7]]")>]
  [<InlineData("[[[[0,7],4],[15,[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")>]
  [<InlineData("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")>]
  let ``Day 18 - tests - split`` (input:string, expectedString:string) =
    let pair = input |> mustParse

    let didSplit,split = pair.split

    let expectingSplit = (input <> expectedString)
    didSplit |> should equal expectingSplit

    split.toString |> should equal expectedString

  [<Theory>]
  [<InlineData("[5,2]", "[5,2]")>]
  [<InlineData("[[[[[4,3],4],3],2],1]",
                   "[[[[0,7],3],2],1]")>] // explode right
  [<InlineData("[1,[2,[3,[4,[4,3]]]]]",
               "[1,[2,[3,[8,0]]]]")>] // explode left
  [<InlineData("[1,[[3,[[4,3],4]],2]]",
               "[1,[[7,[0,7]],2]]")>] // explode both left and right
  [<InlineData("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]",
               "[[[[  0  ,7],4],[7,[[8,4],9]]],[1,1]]")>] // from example
  [<InlineData("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]",
               "[[[[0,7],4],[[7,8],[6,  0  ]]],[8,1]]")>] // from example
  [<InlineData("[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]",
               "[[[[  0,  [3,2]],[3,3]],[4,4]],[5,5]]")>] // from example
  let ``Day 18 - tests - explode`` (input:string, expectedString:string) =
    let expectingExplode = (input <> expectedString)
    let pair = input |> mustParse

    let exploded = pair.explode

    fst exploded |> should equal expectingExplode

    (snd exploded).toString |> should equal (expectedString.Replace(" ", ""))

  [<Theory>]
  [<InlineData("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")>]
  [<InlineData("[[[[[1,1],[2,2]],3],4],5]", "[[[[3,0],5],4],5]")>]
  let ``Day 18 - tests - reduce`` (input:string, expectedReducedResult:string) =
    let pair = input |> mustParse

    pair.reduce.toString |> should equal expectedReducedResult

  [<Theory>]
  [<InlineData("[1,2]", "[[3,4],5]", "[[1,2],[[3,4],5]]")>]
  let ``Day 18 - tests - addition`` (leftInput:string, rightInput:string, expectedString:string) =
    let leftPair = leftInput |> mustParse
    let rightPair = rightInput |> mustParse

    (leftPair + rightPair).toString |> should equal expectedString

  [<Theory>] // adding multiple pairs

  [<InlineData("""
[1,1]
[2,2]
[3,3]
[4,4]
""", """
[[[[1,1],[2,2]],[3,3]],[4,4]]
""")>]

  [<InlineData("""
[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
""", """
[[[[3,0],[5,3]],[4,4]],[5,5]]
""")>]

  [<InlineData("""
[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
""", """
[[[[5,0],[7,4]],[5,5]],[6,6]]
""")>]

  [<InlineData("""
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
""", """
[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
""")>]

  [<InlineData("""
[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
""", """
[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
""")>]

  [<InlineData("""
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
""", """
[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
""")>]

  let ``Day 18 - tests - add multiple pairs`` (input:string, expectedString:string) =

    let pairs = input.Trim().Split("\n")
                |> Array.map mustParse
                |> List.ofArray

    let result =
      pairs.Tail
      |> List.fold (fun (s:Pair) (p:Pair) -> s + p) (pairs.Head)

    result.toString
    |> should equal (expectedString.Trim())
