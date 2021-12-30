namespace AdventOfCode

module Packets =

  module AST =
    type PacketVersion = int32
    type OperationType = int32

    type Packet =
    | LiteralValue of PacketVersion * int64
    | Operation of PacketVersion * OperationType * Packet array

    with
      member x.sumOfPacketVersions =
        match x with
        | LiteralValue(v,_) -> v
        | Operation(v,_,pp) ->
          pp
          |> Array.fold (fun s p -> s + p.sumOfPacketVersions) v

      member x.toJson =
        match x with
        | LiteralValue(_,v) -> sprintf "%d" v
        | Operation(_,operationType,pp) ->
          let operand =
            match operationType with
            | 0 -> (* sum     *) "sum"
            | 1 -> (* product *) "product"
            | 2 -> (* minimum *) "minimum"
            | 3 -> (* maximum *) "maximum"
            // // | 4 -> literal value
            | 5 -> (* greater *) "greater than"
            | 6 -> (* less    *) "less than"
            | 7 -> (* equal   *) "equal to"
            | _ -> failwith "unsupported operation type"

          sprintf """{ "%s": [%s], "value": %d }""" operand (pp |> Array.map (fun p -> p.toJson) |> String.concat ",") (x.value)

      member x.value =
        match x with
        | LiteralValue(_,v) -> v
        | Operation(_,operationType,pp) ->
          match operationType with
          | 0 -> (* sum     *) pp |> Array.sumBy (fun p -> p.value)
          | 1 -> (* product *) pp |> Array.fold (fun product p -> product * p.value) 1
          | 2 -> (* minimum *) pp |> Seq.map (fun p -> p.value) |> Seq.min
          | 3 -> (* maximum *) pp |> Seq.map (fun p -> p.value) |> Seq.max
          // | 4 -> literal value
          | 5 -> (* greater *) if pp.[0].value > pp.[1].value then 1 else 0
          | 6 -> (* less    *) if pp.[0].value < pp.[1].value then 1 else 0
          | 7 -> (* equal   *) if pp.[0].value = pp.[1].value then 1 else 0
          | _ -> failwith "unsupported operation type"

  module Parser =

    open System
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
        // printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        // if reply.Status = Ok then
        //   printfn "%A: Leaving %s (%A) [%A]" stream.Position label reply.Status reply.Result
        // else
        //   printfn "%A: Leaving %s (%A) [%A]" stream.Position label reply.Status reply.Error
        reply

    let bit =
      %[
        %'0';
        %'1';
      ]

    let nonfinalpacket =
      %% '1' ?- +.(bit * qty.[4])
      -%> auto
      <!> "nonfinal packet"

    let finalpacket =
      %% '0' ?- +.(bit * qty.[4])
      -%> auto
      <!> "final packet"

    let payload =
      %% +.(nonfinalpacket * qty.[0..]) -- +.finalpacket
      -|> (fun nn f ->
        nn.Add(f)
        nn |> Array.concat
      )
      <!> "payload"

    let int32FromBinaryChars (cc:char array) =
      Convert.ToInt32(cc |> String, 2)

    let int64FromBinaryChars (cc:char array) =
      Convert.ToInt64(cc |> String, 2)

    let bigIntFromBinaryChars (cc:char array) =
      Convert.ToUInt64(cc |> String, 2) |> bigint

    // let trailingZeros =
    //   '0' * qty.[0..]

    let packetVersion =
      %% +.(bit * qty[3])
      -|> int32FromBinaryChars
      <!> "packetVersion"

    let operationType =
      %% +.(bit * qty[3])
      -|> int32FromBinaryChars
      <!> "operationType"

    let literal =
      %% +.packetVersion -- "100" ?- +.payload // -- trailingZeros
      -|> fun v cc -> (v, int64FromBinaryChars(cc)) |> LiteralValue
      <!> "literal"

    // packetsByLength gets a custom stream parser so that we can parse
    // packets until we have consumed the expected number of bits
    // TODO: validate that we have not consumed too many bits
    let rec packetsByLength n : Parser<_,_> =
      (
        fun stream ->
          let initialState = stream.State
          let initialIndex = stream.Index
          let mutable errors : ErrorMessageList = null
          let results = ResizeArray<Packet>()

          while (stream.Index - initialIndex) < n && errors = null do
            let reply : Reply<Packet> = (packet stream)
            if reply.Error <> null then
              errors <- reply.Error
            else
              results.Add(reply.Result)

          if errors <> null then
            Reply(Error, errors)
          else
            Reply(results.ToArray())
      )
      <!> "packetsByLength"

    // operationBody gets a custom stream parser so we can make a choice
    // of the next thing to parse based on the length type ID
    and operationBody : Parser<_,_> =
      (
        fun stream ->
          let initialState = stream.State

          let reply =
            match stream.Read 1 with
            | "0" ->
              let lengthBits = stream.Read 15
              let length = Convert.ToInt32(lengthBits, 2)
              // printfn "%A: length is 15 bit number %d" stream.Position length
              stream |> packetsByLength length

            | "1" ->
              let nSubPacketsBits = stream.Read 11
              let nSubPackets = Convert.ToInt32(nSubPacketsBits, 2)
              // printfn "%A: length is %d sub packets " stream.Position nSubPackets
              let p =
                %% +.(packet * qty.[nSubPackets])
                -|> fun r -> r
                <!> "operationsByCount"
              p stream
            | _ ->
              let error = expectedString "'0' or '1'"
              Reply(Error, error)

          if reply.Error <> null then
            stream.BacktrackTo initialState
            reply
          else
            reply
      )
      <!> "operationBody"

    and operation : Parser<_,_> =
      %% +.packetVersion -- +.operationType -- +.operationBody
      -|> fun v o b -> Operation(v, o, b)
      <!> "operation"

    and packet =
      %[
        literal;
        operation;
      ]

    let parseBinaryPacket (binaryInput:string) =
      match run packet binaryInput with
      | Success(r, _, _)   -> Result.Ok(r)
      | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

    let parseHexAsPacket (input:string) =
      let b = input |> hex2binary
      // printfn "hex '%s' to binary '%s'" input b
      parseBinaryPacket b
