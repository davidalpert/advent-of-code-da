namespace AdventOfCode

module Day16 =

  open Input
  open Xunit
  open FsUnit.Xunit
  open Packets.AST
  open Packets.Parser

  [<Theory>]
  [<InlineData("D2FE28","110100101111111000101000")>]
  [<InlineData("38006F45291200","00111000000000000110111101000101001010010001001000000000")>]
  [<InlineData("EE00D40C823060","11101110000000001101010000001100100000100011000001100000")>]
  [<InlineData("8A004A801A8002F478","100010100000000001001010100000000001101010000000000000101111010001111000")>]
  [<InlineData("620080001611562C8802118E34","01100010000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100")>]
  [<InlineData("C0015000016115A2E0802F182340","1100000000000001010100000000000000000001011000010001010110100010111000001000000000101111000110000010001101000000")>]
  [<InlineData("A0016C880162017C3686B18A3D4780","101000000000000101101100100010000000000101100010000000010111110000110110100001101011000110001010001111010100011110000000")>]
  let ``Day 16 - tests - hex-to-binary`` (input, expectedBinaryString) =
    input |> hex2binary |> should equal expectedBinaryString

  [<Fact>]
  let ``Day 16 - tests - literal value`` () =
    let input = "D2FE28"
    let expected = LiteralValue(6, 2021)

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result |> should equal expected

  [<Theory>]
  [<InlineData("11010001010", 6, 10)>]
  [<InlineData("0101001000100100", 2, 20)>]
  let ``Day 16 - tests - binary to literal`` (binaryInput, expectedPacketVersion, expectedLiteralValue) =

    let expected = LiteralValue(expectedPacketVersion, expectedLiteralValue)

    match parseBinaryPacket binaryInput with
    | Error(e) -> failwith e
    | Ok(result) ->
      result |> should equal expected

  [<Fact>]
  let ``Day 16 - tests - operation`` () =
    let input = "38006F45291200"
    // 00111000000000000110111101000101001010010001001000000000
    // VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
    // 1234567890123456789012345678901234567890123456789
    //          1         2         3         4
    // 1-3 packet version
    // 4-6 operation type
    // 7 length type id
    // 8-22 length of sub packets
    // 23-33 packet 1
    //       23-25 packet version
    //       26-28 packet type (literal)
    //       29-33 terminal packet
    // 34-49 packet 2
    //       34-36 packet version
    //       37-39 packet type (literal)
    //       40-44 non terminal packet
    //       45-49 non terminal packet


    let expected = Operation (1, 6, [|
      LiteralValue (6, 10);
      LiteralValue (2, 20);
    |])

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result |> should equal expected

  [<Fact>]
  let ``Day 16 - tests - operation example 2`` () =
    let input = "EE00D40C823060"
    // 11101110000000001101010000001100100000100011000001100000
    // VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC
    // 1234567890123456789012345678901234567890123456789
    //          1         2         3         4

    let expected = Operation (7, 3, [|
      LiteralValue (2, 1);
      LiteralValue (4, 2);
      LiteralValue (1, 3);
    |])

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result |> should equal expected

  [<Fact>]
  let ``Day 16 - tests - operation example 3`` () =
    let input = "8A004A801A8002F478"

    let expected = Operation (4, 2, [|
      Operation (1, 2, [|
        Operation (5, 2, [|
          LiteralValue (6, 15)
        |])
      |])
    |])

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result |> should equal expected

  [<Fact>]
  let ``Day 16 - tests - operation example 4`` () =
    let input = "620080001611562C8802118E34"

    let expected = Operation (3, 0, [|
      Operation (0, 0, [|
        LiteralValue (0, 10);
        LiteralValue (5, 11);
      |]);
      Operation (1, 0, [|
        LiteralValue (0, 12);
        LiteralValue (3, 13);
      |]);
    |])

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result |> should equal expected

  [<Fact>]
  let ``Day 16 - tests - operation example 5`` () =
    let input = "C0015000016115A2E0802F182340"

    let expected = Operation  (6, 0, [|
      Operation (0, 0, [|
        LiteralValue (0, 10);
        LiteralValue (6, 11);
      |]);
      Operation (4, 0, [|
        LiteralValue (7, 12);
        LiteralValue (0, 13);
      |]);
    |])

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result |> should equal expected

  [<Fact>]
  let ``Day 16 - tests - operation example 6`` () =
    let input = "A0016C880162017C3686B18A3D4780"

    let expected = Operation  (5, 0, [|
      Operation (1, 0, [|
        Operation (3, 0, [|
          LiteralValue (7, 6);
          LiteralValue (6, 6);
          LiteralValue (5, 12);
          LiteralValue (2, 15);
          LiteralValue (2, 15);
        |])
      |])
    |])

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result |> should equal expected

  [<Theory>]
  [<InlineData("8A004A801A8002F478", 16)>]
  [<InlineData("620080001611562C8802118E34", 12)>]
  [<InlineData("C0015000016115A2E0802F182340", 23)>]
  [<InlineData("A0016C880162017C3686B18A3D4780", 31)>]
  let ``Day 16 - tests - sum of packet versions`` (input, expectedSum) =

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result.sumOfPacketVersions |> should equal expectedSum

  [<Fact>]
  let ``Day 16 - part 1 - sum of packet versions`` () =

    let r = parseHexAsPacket day16data

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result.sumOfPacketVersions |> should equal 963

  [<Theory>]
  [<InlineData("C200B40A82", """{ "sum": [1,2], "value": 3 }""")>]
  [<InlineData("04005AC33890", """{ "product": [6,9], "value": 54 }""")>]
  [<InlineData("880086C3E88112", """{ "minimum": [7,8,9], "value": 7 }""")>]
  [<InlineData("CE00C43D881120", """{ "maximum": [7,8,9], "value": 9 }""")>]
  [<InlineData("D8005AC2A8F0", """{ "less than": [5,15], "value": 1 }""")>]
  [<InlineData("F600BC2D8F", """{ "greater than": [5,15], "value": 0 }""")>]
  [<InlineData("9C005AC2F8F0", """{ "equal to": [5,15], "value": 0 }""")>]
  [<InlineData("9C0141080250320F1802104A08",  """{ "equal to": [{ "sum": [1,3], "value": 4 },{ "product": [2,2], "value": 4 }], "value": 1 }""")>]
  let ``Day 16 - tests - json of packets`` (input, expectedValue) =

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result.toJson |> should equal (expectedValue.ToString().Trim())

  [<Fact(Skip="skip)")>]
  let ``Day 16 - part 2 - json of packets`` () =

    let r = parseHexAsPacket day16data

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result.toJson |> should equal ""

  [<Theory>]
  [<InlineData("C200B40A82", 3)>]
  [<InlineData("04005AC33890", 54)>]
  [<InlineData("880086C3E88112", 7)>]
  [<InlineData("CE00C43D881120", 9)>]
  [<InlineData("D8005AC2A8F0", 1)>]
  [<InlineData("F600BC2D8F", 0)>]
  [<InlineData("9C005AC2F8F0", 0)>]
  [<InlineData("9C0141080250320F1802104A08", 1)>]
  let ``Day 16 - tests - value of packets`` (input, expectedValue) =

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result.value |> should equal (expectedValue |> int64)

  [<Fact>]
  let ``Day 16 - part 2 - value of outermost packet`` () =

    let r = parseHexAsPacket day16data

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result.value |> should equal 1549026292886L