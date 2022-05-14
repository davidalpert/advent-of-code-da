namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module Matchsticks =

  module Parser =

    open System
    open FParsec
    open FParsec.Pipes

    let ch = pchar
    let ws = spaces

    // trace helper to assist with debugging misbehaving parsers
    let (<!>) (p: Parser<_,_>) (label) : Parser<_,_> =
      fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

    type ParsedText = {
      rawText : string
      inMemoryText : string
    }

    // https://stackoverflow.com/a/65479989/8997
    let hexByteToAscii (s:string) =
      System.Convert.ToByte(s,16)
      |> System.Convert.ToChar

    // https://stackoverflow.com/a/65479989/8997
    let charsToString (chars:char[]) =
      new string(chars)

    let escapedSingleBackslash = 
      %% '\\' -? '\\'
      -|> { rawText = [|'\\';'\\'|] |> charsToString; inMemoryText = "\\" }
      // <!> "escapedSingleBackslash"

    let escapedDoubleQuote =
      %% '\\' -? '"'
      -|> { rawText = [|'\\';'"'|] |> charsToString; inMemoryText = "\"" }

    let escapedHexChar =
      %% '\\' -? 'x' -- +.(hex * qty.[2])
      -|> fun code ->
        {
          rawText = "\\x" + (code |> charsToString);
          inMemoryText = [|(code |> charsToString |> hexByteToAscii)|] |> charsToString
        }

    let isUnescapedTextTerminal (c:char) =
      match c with
      | '\\' -> true
      | '"'  -> true // HACK: not an escape char but is a terminator of unescaped text
      | _    -> false

    let isNotUnescapedTerminal (c:char) =
      not (isUnescapedTextTerminal(c))

    let unescapedText =
      %% +.(many1Satisfy isNotUnescapedTerminal)
      -|> fun ss -> { rawText = ss; inMemoryText = ss }

    let pEncodedString =
      %% "\"" -- +.(%[escapedSingleBackslash;escapedDoubleQuote;escapedHexChar;unescapedText] * qty.[0..]) -- "\""
      -%> auto

    let parse (input:string) =
      match run pEncodedString input with
      | Success(r, _, _)   -> Result.Ok(r)
      | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

  exception ParseError of string

  let parseRawString (s:string): (int * int) =

    let parseResult = s |> Parser.parse

    match parseResult with
    | Result.Error(msg) ->
      printfn "%s" msg
      raise (ParseError(msg))
    | Result.Ok(parts) ->

      let folder (lengthRaw,lengthInMemory) (part:Parser.ParsedText) =
        (lengthRaw + part.rawText.Length, lengthInMemory + part.inMemoryText.Length)
        
      let (unquotedRawLength,unquotedInMemoryLength) =
        parts
        |> Seq.fold folder (0,0)

      (unquotedRawLength + 2, unquotedInMemoryLength)

  let calculateDifference (rawStrings:seq<string>) =
    let folder (lengthRaw,lengthInMemory) (partRaw, partInMemory) =
      (lengthRaw + partRaw, lengthInMemory + partInMemory)

    let (totalLengthRaw, totalLengthInMemory) =
      rawStrings
      |> Seq.map parseRawString
      |> Seq.fold folder (0,0)

    totalLengthRaw - totalLengthInMemory

  let encodeRawString (s:string) =
    s.Replace("\\", "\\\\").Replace("\"", "\\\"")

  let calculateDifference2 (rawStrings:seq<string>) =
    let lengthOfEncodedStrings =
      rawStrings
      |> Seq.map encodeRawString
      |> Seq.sumBy (fun s -> s.Length + 2)

    let folder (lengthRaw,lengthInMemory) (partRaw, partInMemory) =
      (lengthRaw + partRaw, lengthInMemory + partInMemory)

    let (totalLengthRaw, totalLengthInMemory) =
      rawStrings
      |> Seq.map parseRawString
      |> Seq.fold folder (0,0)

    // printfn "encodedLength %d - rawLength %d" lengthOfEncodedStrings totalLengthRaw

    lengthOfEncodedStrings - totalLengthRaw
