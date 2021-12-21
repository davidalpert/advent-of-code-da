namespace AdventOfCode

module Octopods =

  let inline charToInt c = int c - int '0'
  let inline intToChar i = sprintf "%d" i 

  let adjacents (r:int) (c:int) =
    seq [
      (-1,-1);(-1, 0);(-1, 1);
      ( 0,-1);        (-1, 1);
      ( 1,-1);( 1, 0);( 1, 1);
    ]
    |> Seq.map (fun (mr,mc) -> (r + mr, c + mc))

  let validAdjacents (dim:int) (r:int) (c:int) =
    adjacents r c
    |> Seq.filter (fun (r,c) -> 0 <= r && r < dim && 0 <= c && c < dim)

  let inline isFlashing (i:int) = i > 9

  let numberOfFlashingAdjacents (dim:int) (r:int) (c:int) (grid:int[][]) =
    if grid.[r].[c] |> isFlashing then
      0
    else
      validAdjacents dim r c
      |> Seq.filter (fun (nr,nc) -> grid.[nr].[nc] |> isFlashing)
      |> Seq.length

  // let inline indicesForDim (dim:int) =
  //   seq { 0 .. (dim - 1) } |> Array.ofSeq

  let mapGridi (fn:int -> int -> int -> int) (cells:int[][]) =
    cells |> Array.mapi (fun r row ->
      row |> Array.mapi (fun c cell ->
        fn r c cell
      )
    )

  let existsInGrid (fn:int -> bool) (cells:int[][]) =
    cells |> Seq.exists (fun row ->
      row |> Seq.exists (fun cell ->
        fn cell
      )
    )

  type Cavern(dim:int,cells:int[][]) =

    member x.step =
      let increased =
        cells |> mapGridi (fun r c cell -> cell + 1)

      let hasNewFlashes (grid:int[][]) =
        grid |> existsInGrid isFlashing

      let rec resolveFlashes (grid:int[][]) =
        let g2 =
          grid
          |> mapGridi (fun r c cell ->


          )




        let applyFlashes r c cell =
          validAdjacents r c
          |> Seq.filter (fun (nr,nc) ->  grid[nr][nc] + 1
          )

        match hasUnflashed grid with
        | false -> grid
        | true  -> grid
                   |> mapGridI (fun r c cell ->
                     cell + grid
                            |> flashingAdjacents dim r c
                            |> Seq.sum
                   )



      Cavern(dim,increased)

    member x.afterStep (n:int) =
      match sign n with
      | 1 -> seq { 1 .. n}
             |> Seq.fold (fun (s:Cavern) _ -> s.step) x
      | _ -> x

    member x.cells =
      cells

    member x.toString =
      let s =
        cells
        |> Array.map (fun row ->
          row
          |> Array.map intToChar
          |> String.concat ""
        )
        |> String.concat "\n"

      "\n" + s + "\n"

    // member x.rows = seq { 0 .. (dim - 1) } |> Array.ofSeq

    // static member cols = seq { 0 .. 9 } |> Array.ofSeq

    static member ofDim (dim:int) (cells:int[][]) =
      Cavern(dim,cells)

    static member fromInput (input:string) =
      let rows = input.Trim().Split("\n")
      let dim = rows.Length

      rows
      |> Seq.map (fun l -> l.ToCharArray() |> Array.map charToInt)
      |> Array.ofSeq
      |> Cavern.ofDim dim




