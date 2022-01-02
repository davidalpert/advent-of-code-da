namespace AdventOfCode

module Day19 =

  open Input
  open Xunit
  open FsUnit.Xunit
  open BeaconScanner
  open BeaconScanner.Model

  [<Fact>]
  let ``Day 19 - tests - mustParse`` () =

    let sampleData = """
--- scanner 0 ---
404,-588,-901
528,-643,409

--- scanner 1 ---
686,422,578
605,423,415

--- scanner 2 ---
649,640,665
682,-795,504

--- scanner 3 ---
-589,542,597
605,-692,669

--- scanner 4 ---
727,592,562
-293,-554,779
"""
    sampleData |> Parser.mustParse // |> should equal []

  // In total, each scanner could be in any of 24 different orientations: facing positive or negative x, y, or z, and considering any of four directions "up" from that facing.
  [<Fact>]
  let ``Day 19 - tests - number of orientations`` () =
    Rotations.allRotationMatrices.Length
    |> should equal 24

  // [<Theory>]
  // [<InlineData("-1,-1,1", "1,-1,1")>]
  // [<InlineData("-2,-2,2", "2,-2,2")>]
  // [<InlineData("-3,-3,3", "3,-3,3")>]
  // [<InlineData("-2,-3,1", "2,-1,3")>]
  // [<InlineData("5,6,-4",  "-5,4,-6")>]
  // [<InlineData("8,0,7",   "-8,-7,0")>]
  // let ``Day 19 - tests - transform a coordinate`` (input:string, expectedString:string) =
  //   let c = Parser.mustParseCoordinate input

  //   // let m = Rotations.allRotationMatrices.[1]

  //   // (c.rotateByMatrix m).string |> should equal expectedString

  //   Rotations.allRotationMatrices
  //   |> Seq.findIndex (fun m -> (c.rotateByMatrix m).string = expectedString)
  //   |> should equal -1

  // [<Fact>]
  // let ``Day 19 - tests - transform a coordinate`` () =
  //   let r = Parser.mustParse day19simpleInput

  //   r
  //   |> List.iter (fun sr ->
  //     sr.beacons
  //   )

  //   // let m = Rotations.allRotationMatrices.[1]

  //   // (c.rotateByMatrix m).string |> should equal expectedString

  //   Rotations.allRotationMatrices
  //   |> Seq.findIndex (fun m -> (c.rotateByMatrix m).string = expectedString)
  //   |> should equal -1




