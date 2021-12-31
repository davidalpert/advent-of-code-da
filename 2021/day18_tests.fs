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
  [<InlineData("[1,2]", "[[3,4],5]", "[[1,2],[[3,4],5]]")>]
  let ``Day 18 - tests - addition`` (leftInput:string, rightInput:string, expectedString:string) =
    let leftPair = leftInput |> mustParse
    let rightPair = rightInput |> mustParse

    (leftPair + rightPair).toString |> should equal expectedString
