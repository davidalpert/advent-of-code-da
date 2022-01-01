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

  [<Theory>]
  [<InlineData("[9,1]", 29)>]
  [<InlineData("[1,9]", 21)>]
  [<InlineData("[[9,1],[1,9]]", 129)>]
  [<InlineData("[[1,2],[[3,4],5]]", 143)>]
  [<InlineData("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384)>]
  [<InlineData("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445)>]
  [<InlineData("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791)>]
  [<InlineData("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137)>]
  [<InlineData("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488)>]
  let ``Day 18 - tests - magnitude`` (input:string, expectedMagnitude:int64) =
    let pair = input |> mustParse

    pair.magnitude |> should equal expectedMagnitude

  [<Theory>]
  [<InlineData("""
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
""", """
[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]
""", 4140)>]

  let ``Day 18 - part 1 - sample`` (input:string, expectedSum:string, expectedMagnitude:int64) =

    let pairs = input.Trim().Split("\n")
                |> Array.map mustParse
                |> List.ofArray

    let result =
      pairs.Tail
      |> List.fold (fun (s:Pair) (p:Pair) -> s + p) (pairs.Head)

    result.toString |> should equal (expectedSum.Trim())

    result.magnitude |> should equal expectedMagnitude

  [<Fact>]
  let ``Day 18 - part 1 - calculation`` ()  =

    let pairs = day18input.Trim().Split("\n")
                |> Array.map mustParse
                |> List.ofArray

    let result =
      pairs.Tail
      |> List.fold (fun (s:Pair) (p:Pair) -> s + p) (pairs.Head)

    result.toString |> should equal ("[[[[6,6],[6,6]],[[6,7],[7,7]]],[[[0,7],[7,7]],[[7,7],[7,8]]]]")

    result.magnitude |> should equal (3734 |> int64)

  [<Fact>]
  let ``Day 18 - part 2 - oneWayCombinationsOf`` ()  =
    let lst = [1;2;3]

    lst |> oneWayCombinationsOf |> should equal [[3]; [3; 2]; [3; 2; 1]; [3; 1]; [2]; [2; 1]; [1]]

  [<Fact>]
  let ``Day 18 - part 2 - allCombinationsOfSize2`` ()  =
    let lst = [1;2;3]

    lst |> allCombinationsOfSize2 |> should equal [
      (1, 2);
      (1, 3); 
      (2, 1);
      (2, 3);
      (3, 1); 
      (3, 2);
    ]

  [<Fact>]
  let ``Day 18 - part 2 - example`` ()  =

    let pairs = day18example.Trim().Split("\n")
                |> Array.map mustParse
                |> List.ofArray

    pairs
    |> allCombinationsOfSize2
    |> List.map (fun (x,y) -> x + y)
    |> List.map (fun p -> p.magnitude)
    |> List.max
    |> should equal ( 3993 |> int64 )
