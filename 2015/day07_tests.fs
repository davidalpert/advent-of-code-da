namespace AdventOfCode.Tests

module Day07 =

  open AdventOfCode.Input
  open AdventOfCode.AssemblyRequired
  open System.Collections.Generic
  open Xunit
  open FsUnit.Xunit

  let dictToList<'a,'b> (d:IDictionary<'a,'b>) =
    d
    |> Seq.map (fun pair -> (pair.Key, pair.Value))
    |> List.ofSeq

  let expectedMapToSortedDictionary (s:string) =
    let expectedMap = SortedDictionary<string,uint16>()

    s
    |> splitToTrimmedLines
    |> Seq.map (fun x ->
      let parts = x.Split(":")
      (parts.[0],(parts.[1] |> uint16))
    )
    |> Seq.iter (fun (c,i) -> expectedMap.Add(c,i) )

    expectedMap

  [<Theory>]
  [<InlineData("123 -> x", "x:123")>] 
  [<InlineData("123 -> x\nx LSHIFT 2 -> f", "f:492\nx:123")>] 
  [<InlineData("""
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i""", """d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456""")>] 
  let ``Day 7 - part 1 - tests`` (input:string, expected:string) =
    let expectedCircuit = expected |> expectedMapToSortedDictionary

    input
    |> splitToTrimmedLines
    |> assembleCircuit
    |> dictToList
    |> should matchList (expectedCircuit |> dictToList)

  [<Theory>]
  [<InlineData(1,1,2)>]
  [<InlineData(1,2,4)>]
  [<InlineData(123,2,492)>]
  let ``bitwise <<< test``(x,y,e) =
    x <<< y |> should equal e

  [<Fact>]
  let ``Day 7 - part 1 - calculation``() =
    let circuitMap =
      day07input
      |> splitToTrimmedLines
      |> assembleCircuit

    circuitMap.["a"] |> should equal -1
