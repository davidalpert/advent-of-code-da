namespace AdventOfCode

module NavigationModel =

  open System.Collections.Generic

  let validDelimeters =
    [
      '(',')';
      '[',']';
      '{','}';
      '<','>';
    ]
    |> dict

  type Chunk =
  | ValidChunk of startingChar:char * children:Chunk[] * endingChar:char
  | CorruptedChunk of startingChar:char * children:Chunk[] * expectedChar:char * actualChar:char
  | IncompleteChunk of startingChar:char * children:Chunk[]

  and Line =
  | ValidLine of children:Chunk[]
  | CorruptedLine of children:Chunk[] * corrruptChunk:Chunk
  | IncompleteLine of children:Chunk[] * incompleteChunk:Chunk

module NavigationParser =

  open System
  open FParsec
  open FParsec.Pipes
  open NavigationModel

  let pLine : Parser<Line,unit> =
    preturn (ValidLine([||]))

  let parseChunk (input:string) =
    match run pLine input with
    | Success(r, _, _)   -> Result.Ok(r)
    | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

  let a = 1