namespace AdventOfCode

module Day14 =

  open Input
  open Xunit
  open FsUnit.Xunit
  open Polymerization

  [<Fact>]
  let ``Day 14 - tests - parse input`` () =
    let r = parse day14sample

    r.toString |> should equal "NNCB"
    r.rules |> Seq.length |> should equal 16

  [<Theory>]
  [<InlineData(1,"NCNBCHB")>]
  [<InlineData(2,"NBCCNBBBCBHCB")>]
  [<InlineData(3,"NBBBCNCCNBBNBNBBCHBHHBCHB")>]
  [<InlineData(4,"NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")>]
  let ``Day 14 - tests - sample data after step`` (n,expectedPolymer) =
    let r = parse day14sample

    (r.polymerAfterNSteps n) |> should equal expectedPolymer

  [<Fact>]
  let ``Day 14 - part 1 - sample data`` () =
    let r = parse day14sample

    let p = r.afterNSteps 10

    p.differenceBetweenMaxAndMinNumberOfOccurances
    |> should equal 1588

  [<Fact>]
  let ``Day 14 - part 1 - calculations`` () =
    let r = parse day14data

    let p = r.afterNSteps 10

    p.differenceBetweenMaxAndMinNumberOfOccurances
    |> should equal 3831

  [<Fact>]
  let ``Day 14 - part 2 - calculations`` () =
    let r = parse day14data

    let p = r.afterNSteps 40

    p.differenceBetweenMaxAndMinNumberOfOccurances
    |> should equal 3831