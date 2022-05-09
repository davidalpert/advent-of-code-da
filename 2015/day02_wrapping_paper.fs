namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module WrappingPaper =

  type GiftDimensions = {
    l: int
    w: int
    h: int
  }

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
    let sides = [|
      g.l * g.w;
      g.w * g.h;
      g.h * g.l;
    |]

    // slack equal to the smallest side
    let smallestSide = sides |> Array.min

    // double the area of each side + the slack
    ((sides |> Array.sum) * 2) + smallestSide