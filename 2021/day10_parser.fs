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

  let scoreByIllegalChar = 
    [
      ')',     3 |> int64;
      ']',    57 |> int64;
      '}',  1197 |> int64;
      '>', 25137 |> int64;
    ]
    |> dict

  type Chunk =
  | ValidChunk of startingChar:char * children:Chunk seq * endingChar:char
  | CorruptedChunk of startingChar:char * children:Chunk seq * expectedChar:char * actualChar:char * unparsed:string
  | IncompleteChunk of startingChar:char * children:Chunk seq
  with
    member x.toAnnotatedString =
      let ss (cc:Chunk seq) =
        cc
        |> Seq.map (fun c -> c.toAnnotatedString)
        |> String.concat ""

      match x with
      | ValidChunk(s,cc,e)         -> sprintf "v:%c%s%c" s (ss cc) e
      | CorruptedChunk(s,cc,e,a,u) ->
        let p = sprintf "c:%c%s%c expected '%c' but got '%c'%s" s (ss cc) a e a
        match u.Length with
        | 0 -> p ""
        | _ -> p (sprintf " ignoring: '%s'" u)
      | IncompleteChunk(s,cc)      ->
        sprintf "i:%c%s" s (ss cc)

    static member tryFindCorrupted (cc:Chunk seq) =
      let x =
        cc
        |> Seq.map (fun c -> c.tryFindCorrupt)
        |> Seq.filter (fun (r:Chunk option) -> r.IsSome)

      match x |> Seq.tryHead with
      | Some(h) -> h
      | None    -> None

    member x.tryFindCorrupt =
      match x with
      | ValidChunk(_,cc,_)        -> Chunk.tryFindCorrupted cc
      | CorruptedChunk(_,_,_,_,_) -> Some(x)
      | IncompleteChunk(_,cc)     -> Chunk.tryFindCorrupted cc

  and Line =
  | ValidLine of children:Chunk seq
  | CorruptedLine of children:Chunk seq * corrruptChunk:Chunk
  | IncompleteLine of children:Chunk seq * incompleteChunk:Chunk
  with
    member x.toAnnotatedString =
      match x with
      | ValidLine(cc)         -> sprintf "Valid      | %s" (cc |> Seq.map (fun c -> c.toAnnotatedString) |> String.concat " | ")
      | CorruptedLine(cc, x)  -> sprintf "Corrupted  | %s" (cc |> Seq.map (fun c -> c.toAnnotatedString) |> String.concat " | ")
      | IncompleteLine(cc, x) -> sprintf "Incomplete | %s" (cc |> Seq.map (fun c -> c.toAnnotatedString) |> String.concat " | ")

    static member lift (chunks:Chunk seq) =
      // printfn "lifting chunks: %s" (chunks |> Seq.map (fun c -> c.toAnnotatedString) |> String.concat "; ")

      let allValid (cc:Chunk seq) =
        cc |> Seq.forall  (fun c -> match c with ValidChunk(_,_,_) -> true | _ -> false)

      let tryFindIncomplete (cc:Chunk seq) =
        cc |> Seq.tryFind (fun c -> match c with IncompleteChunk(_,_) -> true | _ -> true)

      match allValid chunks with
      | true  -> ValidLine(chunks)
      | false -> match Chunk.tryFindCorrupted chunks with
                 | Some(c) -> CorruptedLine(chunks, c)
                 | None -> match tryFindIncomplete chunks with
                           | Some(c) -> IncompleteLine(chunks, c)
                           | None -> failwith (sprintf "something went wrong %s" (chunks |> Seq.map (fun x -> x.toAnnotatedString) |> String.concat " | "))

  type Subsytem(lines:Line seq) =
    member x.toAnnotatedString =
      lines
      |> Seq.map (fun l -> l.toAnnotatedString)
      |> String.concat "\n"

    member x.totalSyntaxScore =
      lines
      |> Seq.filter (fun l -> match l with | CorruptedLine(_,_) -> true | _ -> false)
      |> Seq.map (fun l ->
        match l with
        | CorruptedLine(_,c) -> 
            match c with
            | CorruptedChunk(_,_,_,a,_) -> scoreByIllegalChar.[a]
            | _ -> 0L
        | _ -> 0L
      )
      // |> Seq.map (fun i ->
      //   printfn "syntaxScore: %d" i
      //   i
      // )
      |> Seq.sum

module NavigationParser =

  open System
  open FParsec
  open FParsec.Pipes
  open NavigationModel

  let ch = pchar
  let ws = spaces
  // let nl = pchar '\n'

  let (<!>) (p: Parser<_,_>) (label,startingChar,endingChar) : Parser<_,_> =
    fun stream ->
        // printfn "%A: Entering %s %c..%c" stream.Position label startingChar endingChar
        let reply = p stream
        // printfn "%A: Leaving %s %c..%c (%A)" stream.Position label startingChar endingChar reply.Status
        reply

  let rec pAnyValidChunk : Parser<Chunk,unit> =
    choice (
      validDelimeters
      |> Seq.map (fun pair ->
        (
          %% +.(ch pair.Key) -- +.(%[pAnyValidChunk]*qty.[0..]) -? +.(ch pair.Value)
          -|> fun s c e -> ValidChunk(s,c,e)
        ) <!> ("pValidChunk",pair.Key,pair.Value)
      )
    )

  and pAnyIncompleteChunk : Parser<Chunk,unit> =
    choice (
      validDelimeters
      |> Seq.map (fun pair ->
        (
          %% +.(ch pair.Key) -- +.(%[pChunk]*qty.[0..])
          -|> fun s c -> IncompleteChunk(s,c)
        )
      )
    )

  and pOneCorruptedChunk (startingChar:char): Parser<Chunk,unit> =
    let expectedEndingChar = validDelimeters.[startingChar]

    choice (
      validDelimeters
      |> Seq.filter (fun pair -> pair.Key <> startingChar)
      |> Seq.map (fun pair -> 
        (
          %% (ch startingChar) -- +.(%[pAnyValidChunk]*qty.[0..]) -? (ch pair.Value) -- +.(restOfLine false)
          -|> fun c rest -> CorruptedChunk(startingChar,c,expectedEndingChar,pair.Value, rest)
        ) <!> ("pOneCorruptedChunk",startingChar,pair.Value)
      )
    )

  and pAnyCorruptedChunk : Parser<Chunk,unit> =
    choice (
      validDelimeters
      |> Seq.map (fun pair -> (pOneCorruptedChunk pair.Key))
    )

  and pChunk : Parser<Chunk,unit> =
    choice [
      pAnyValidChunk;
      pAnyCorruptedChunk;
      pAnyIncompleteChunk;
    ]

  let pLine : Parser<Line,unit> =
    %% ws -- +.(pChunk * qty.[1..]) -- ws
    -|> Line.lift

  let pSubsystem : Parser<Subsytem,unit> =
    %% ws -- +.(pLine * qty.[1..]) -- ws
    -|> Subsytem

  let parseChunk (input:string) =
    match run pChunk input with
    | Success(r, _, _)   -> Result.Ok(r)
    | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

  let parseLine (input:string) =
    match run pLine input with
    | Success(r, _, _)   -> Result.Ok(r)
    | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

  let parseSubsystem (input:string) =
    match run pSubsystem input with
    | Success(r, _, _)   -> Result.Ok(r)
    | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

  let a = 1