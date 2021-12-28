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

  [<Fact>]
  let ``Day 16 - tests - operation`` () =
    let input = "38006F45291200"
    let expected = LiteralValue(6, 2021)

    let r = parseHexAsPacket input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) ->
      result |> should equal expected
