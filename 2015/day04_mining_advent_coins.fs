namespace AdventOfCode

open AdventOfCode.Input

module MiningAdventCoins =

  open System.Security.Cryptography
  open System.Text

  // http://www.fssnip.net/3D/title/MD5-hash
  let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

  let firstCoinSuffix (key:string) =
    // A function that takes in the current state and returns an option tuple of the next element of the sequence and the next state value.
    let generator (state:int) =
      let fullKey = sprintf "%s%d" key state
      let hash = fullKey |> System.Text.Encoding.ASCII.GetBytes |> md5

      match hash.ToString().Substring(0,5) with
      | "00000" -> None
      | _ -> Some(state+1, state+1)

    let n = Seq.unfold generator 0

    n
    |> Seq.last
