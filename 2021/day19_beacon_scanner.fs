namespace AdventOfCode

module BeaconScanner =

  open FSharp.Stats

  // Due to magnetic alignment, each scanner is rotated some integer number of 90-degree turns around all of the x, y, and z axes. That is, one scanner might call a direction positive x, while another scanner might call that direction negative y. Or, two scanners might agree on which direction is positive x, but one scanner might be upside-down from the perspective of the other scanner. In total, each scanner could be in any of 24 different orientations: facing positive or negative x, y, or z, and considering any of four directions "up" from that facing.
  module Rotations =

    // simplified sin and cos functions locked to n 90 degree
    // turns let us work with integers instead of floats
    let sinN n =
      [| 0;  1;  0; -1|].[n % 3]

    let cosN n =
      [| 1;  0; -1;  0|].[n % 3]

    let rotationMatrixAroundX n orientation =
      Matrix.Generic.ofSeq [|
        [| orientation ;           0 ;           0 ; |];
        [|           0 ;      cosN n ;   - (sinN n); |];
        [|           0 ;      sinN n ;      cosN n ; |];
      |]

    let rotationMatrixAroundY n orientation =
      Matrix.Generic.ofSeq [|
        [|      cosN n ;           0 ;      sinN n ; |];
        [|           0 ; orientation ;           0 ; |];
        [|   - (sinN n);           0 ;      cosN n ; |];
      |]

    let rotationMatrixAroundZ n orientation =
      Matrix.Generic.ofSeq [|
        [|      cosN n ;   - (sinN n);           0 ; |];
        [|      sinN n ;      cosN n ;           0 ; |];
        [|           0 ;           0 ; orientation ; |];
      |]

    let rotationMatrixFns = [|
      rotationMatrixAroundX;
      rotationMatrixAroundY;
      rotationMatrixAroundZ;
    |]

    // x, -x, x rotate 90, -x rotate 90, x rotate 180, -x rotate 180, x rotate 270, -x rotate 270, y, -y, etc
    let allRotationMatrices = 
      rotationMatrixFns
      |> Array.collect (fun fn ->
        [| for n in 0 .. 3 -> n |] 
        |> Array.map (fun n ->
          [|
            (n,  1); //right side up (no change)
            (n, -1); // up side down (inverted along the axis of rotation)
          |]
          |> Array.map (fun (n,o) -> fn n o)
        )
        |> Array.concat
      )

    // let allRotationsOf (p:int array) =
    //   allRotationMatrices
    //   |> Array.map 

  module Model =
    type Coordinate = {
      v:Vector<int>
    }
    with
      // static member lift x y z = { x = x; y = y; z = z; }
      static member lift (x:int) (y:int) (z:int) = {
        v = ([| x; y; z; |] |> Vector.Generic.ofArray)
      }

      static member unkonwn : Coordinate option = None

      member c.string = sprintf "%d,%d,%d" c.v.[0] c.v.[1] c.v.[2]

      member c.rotateByMatrix (m:int Matrix) =
        {
          v = m * c.v
        }

      // member c.rotate fn = 

    type ScannerReport = {
      number: int
      position: Coordinate option
      beacons: Coordinate list
    }
    with
      static member lift n b  = { number = n; position = Coordinate.unkonwn; beacons = b |> List.ofSeq; }

    type ScanReport = ScannerReport list

  module Parser =

    open FParsec
    open FParsec.Pipes
    open Model
    open utils
    
    let private pint = p<int>
    let ch = pchar
    let ws = spaces

    let private coordinate =
      %% +.pint -- ',' -- +.pint -- ',' -- +.pint -- ws
      -|> Coordinate.lift

    let private scannerReport =
      %% "--- scanner " -- +.pint -- " ---" -- ws
      -- +.(coordinate * qty[1..])
      -|> ScannerReport.lift

    let private scanReport : Parser<ScanReport,unit> =
      many scannerReport

    let parse (input:string) =
      match run scanReport (input.Trim()) with
      | Success(r, _, _) -> Result.Ok(r)
      | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

    let mustParse (input:string) =
      match parse input with
      | Result.Ok(r) -> r
      | Result.Error(msg) -> failwith msg

    let mustParseCoordinate (input:string) =
      match run coordinate (input.Trim()) with
      | Success(r, _, _) -> r
      | Failure(errorMsg, _, _) -> failwith errorMsg
