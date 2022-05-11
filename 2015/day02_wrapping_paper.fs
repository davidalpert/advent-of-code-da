namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module WrappingPaper =

  type GiftDimensions = {
    l: int
    w: int
    h: int
  }
  with
    member this.sides = [|
        (this.l, this.w);
        (this.w, this.h);
        (this.h, this.l);
      |]
    
    member this.sideAreas =
      this.sides
      |> Array.map (fun (x,y) -> x*y)

    member this.sidePerimeters =
      this.sides
      |> Array.map (fun (x,y) -> x + x + y + y)

    member this.volume = this.l * this.w * this.h

  let singleGiftToDimensions (s:string) =
    let dd = s.Split("x")
    {
      l = dd.[0] |> int32;
      w = dd.[1] |> int32;
      h = dd.[2] |> int32;
    }

  let inputToGiftDimensions (rawInput:string) : seq<GiftDimensions> =
    rawInput
    |> splitToTrimmedLines
    |> Seq.map singleGiftToDimensions

  let squareFootagePlusSlack (g:GiftDimensions) =
    // square footage rquired: 2*l*w + 2*w*h + 2*h*l 
    // slack equal to the smallest side
    let smallestSide = g.sideAreas |> Array.min

    // double the area of each side + the slack
    ((g.sideAreas |> Array.sum) * 2) + smallestSide

  let lengthToWrap (g:GiftDimensions) =
    g.sidePerimeters
    |> Seq.min

  let lengthForBow (g:GiftDimensions) =
    g.volume

  let lengthOfRibbonRequired (g:GiftDimensions) =
    (g |> lengthToWrap) + (g |> lengthForBow)