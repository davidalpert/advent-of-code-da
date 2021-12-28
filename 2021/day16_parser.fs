namespace AdventOfCode

module Packets =

  module AST =
    type PacketVersion = int32
    type OperationType = int32

    type Packet =
    | LiteralValue of PacketVersion * int32
    | Operation of PacketVersion * OperationType * Packet array

    type OperationLength =
    | LengthInBits of int32
    | NumberOfSubPackets of int32

  module Parser =

    open System
    open System.Collections.Generic
    open FParsec
    open FParsec.Pipes
    open AST

    let hexValues =
      [
        '0', "0000";
        '1', "0001";
        '2', "0010";
        '3', "0011";
        '4', "0100";
        '5', "0101";
        '6', "0110";
        '7', "0111";
        '8', "1000";
        '9', "1001";
        'A', "1010";
        'B', "1011";
        'C', "1100";
        'D', "1101";
        'E', "1110";
        'F', "1111";
      ] |> dict

    let hex2binary (input:string) =
      input.ToCharArray()
      |> Array.map (fun c ->
        hexValues[c]
      )
      |> String.concat ""

    let ch = pchar
    let ws = spaces

    // trace helper to assist with debugging misbehaving parsers
    let (<!>) (p: Parser<_,_>) (label) : Parser<_,_> =
      fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

    let bit =
      %[
        %'0';
        %'1';
      ]

    let nonfinalpacket =
      %% '1' ?- +.(bit * qty.[4])
      -%> auto
      // <!> "nonfinal packet"

    let finalpacket =
      %% '0' ?- +.(bit * qty.[4])
      -%> auto
      // <!> "final packet"

    let payload =
      %% +.(nonfinalpacket * qty.[0..]) -- +.finalpacket
      -|> (fun nn f ->
        nn.Add(f)
        nn |> Array.concat
      )
      // <!> "payload"

    let int32FromBinaryChars (cc:char array) =
      Convert.ToInt32(cc |> String, 2)

    let int64FromBinaryChars (cc:char array) =
      Convert.ToInt64(cc |> String, 2)

    let trailingZeros =
      '0' * qty.[0..]

    let version =
      %% +.(bit * qty[3])
      -|> int32FromBinaryChars
      // <!> "version"

    let literal =
      %% +.version -? "100" -- +.payload -- trailingZeros
      -|> fun v cc -> (v, int32FromBinaryChars(cc)) |> LiteralValue
      // <!> "literal"

    let operationLength =
      %[
        %% '0' -- +.(bit * qty[15]) -|> int32FromBinaryChars |>> LengthInBits;
        %% '1' -- +.(bit * qty[11]) -|> int32FromBinaryChars |>> NumberOfSubPackets;
      ]

    let rec operationsByLength n : Parser<_,_> =
      let p = %% +.(packet * qty.[1..])
              -|> fun pp -> pp
      
      let operationParsingError (i:int64) (e:ParserError) (msg:string) =
        messageError (sprintf "error parsing operation at %d" (i + e.Position.Index))

      (
        fun stream ->
          let initialIndex = stream.Index
          let ops = stream.Read n
          match run p ops with
          | Success(r, _, _) -> Reply(r |> Array.ofSeq)
          | Failure(errorMsg, parseError, _) ->
              Reply(Error, operationParsingError initialIndex parseError errorMsg)
      )
      <!> "operationsByLength"

    and operation : Parser<_,_> =
      (
        fun stream ->
          let initialState = stream.State

          let versionBits = stream.Read 3
          let version = Convert.ToInt32(versionBits, 2)
  
          let operationBits = stream.Read 3
          let operation = Convert.ToInt32(operationBits, 2)

          match stream.Read 1 with
          | "0" ->
            let lengthBits = stream.Read 15
            let length = Convert.ToInt32(lengthBits, 2)
            let rest = stream.Read length
            match rest |> run (operationsByLength length) with
            | Success(r,_,_) -> Reply(Operation(version, operation, r))
            | Failure(errorMsg,_,_) ->
              stream.BacktrackTo(initialState)
              Reply(Error, messageError errorMsg)

          | "1" ->
            let nSubPacketsBits = stream.Read 11
            let nSubPackets = Convert.ToInt32(nSubPacketsBits, 2)
            let p =
              %% +.(packet * qty.[nSubPackets])
              -|> fun r -> r
              <!> "operationsByCount"
            let reply = p stream
            Reply(Operation(version, operation, reply.Result))
          | _ ->
            let error = expectedString "'0' or '1'"
            Reply(Error, error)
      )
      <!> "operation"

    and packet =
      %[
        literal;
        operation;
      ]

    let parseHexAsPacket (input:string) =
      match run packet (input |> hex2binary) with
      | Success(r, _, _)   -> Result.Ok(r)
      | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

    let parseHexAsOperation (input:string) =
      match run operation (input |> hex2binary) with
      | Success(r, _, _)   -> Result.Ok(r)
      | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

