namespace AdventOfCode

module TrenchMap =

  open System
  open FSharp.Stats

  let toBit c =
    match c with
    | '#' -> 1
    | '.' -> 0
    | _ -> failwith $"toBit: unexpected char '{c}'"

  let binaryStringToInt input = Convert.ToInt32(input, 2);

  type ImageCoord = {
    x : int
    y : int
    c : char
  }
  with
    member c.render =
      $"(x:{c.x},y:{c.y},c:{c.c})"

  type Image = {
    EnhancementAlgorithm : char array
    Content : ImageCoord [,]
  }
  with
    member i.getSurroundingCoords x y =
      [
        (-1, -1); (0, -1); (1, -1);
        (-1,  0); (0,  0); (1,  0);
        (-1,  1); (0,  1); (1,  1);
      ]
      |> Seq.map (fun (mx, my) -> (x+mx, y+my))

      member i.dimensions =
        i.Content |> Array2D.length1, i.Content |> Array2D.length2

      member i.render =
        let maxX,maxY = i.dimensions
        seq { 0 .. (maxY-1) }
        |> Seq.map (fun y ->
          seq { 0 .. (maxX-1) }
          |> Seq.map (fun x ->
            i.Content[x,y].c
          )
          |> Array.ofSeq
          |> String
        )
        |> String.concat "\n"

      member i.getSurroundingChars x y =
        i.getSurroundingCoords x y
        |> Seq.map (fun (x,y) ->
          try
            i.Content[x,y].c
          with
            | _ -> '.'
        )
        |> Array.ofSeq

      member i.charStringFor x y =
        i.getSurroundingChars x y
        |> String

      member i.bitStringFor x y =
        i.getSurroundingChars x y
        |> Array.map toBit
        |> Array.map (fun b -> $"{b}")
        |> String.concat ""

      member i.decimalFor x y =
        i.bitStringFor x y |> binaryStringToInt

  module Parser =
    let mustParse (input:string) =
      let lines = input.Trim().Split("\n")
      let algorithm = lines.[0].ToCharArray()
      let inputLines = lines.[2..] |> Array.map (fun s -> s.ToCharArray())

      let charFor x y =
        {
          x = x; y = y; c = inputLines[y][x]
        }

      let inputImage = Array2D.init inputLines.[0].Length inputLines.Length charFor

      {
        EnhancementAlgorithm = algorithm;
        Content = inputImage;
      }