namespace AdventOfCode

open AdventOfCode.Input

module NaughtyDetector =

  open System
  open System.Security.Cryptography
  open System.Text

  // A nice string is one with all of the following properties:

  // - It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
  let containsAtLeastNVowels (n:int) (s:string) : bool =
    let vowels =
      s
      |> Seq.filter (fun c -> match c with |'a'|'e'|'i'|'o'|'u' -> true | _ -> false)
    (vowels |> Seq.length) >= n

  // - It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
  let atLeastOneDuplicatedLetter (nTimes:int) (s:string) : bool =
    let found =
      s
      |> Seq.windowed nTimes
      |> Seq.tryFind (fun s -> s.[0] = s.[1])

    found.IsSome

  // - It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
  let doesNotMatch (ss : seq<string>) (s:string) : bool =
    let found =
      ss
      |> Seq.tryFind (fun c -> s.Contains(c))

    found.IsNone

  // It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
  let containsPairOfLetters (s:string) : bool =
    let found0 =
      s
      |> Seq.chunkBySize 2
      |> Seq.filter (fun cc -> cc.Length = 2 )
      |> Seq.countBy (fun cc -> (cc.[0],cc.[1]))
      |> Seq.tryFind (fun (_,n) -> n > 1)

    let found1 =
      s
      |> Seq.skip 1
      |> Seq.chunkBySize 2
      |> Seq.filter (fun cc -> cc.Length = 2 )
      |> Seq.countBy (fun cc -> (cc.[0],cc.[1]))
      |> Seq.tryFind (fun (_,n) -> n > 1)

    found0.IsSome || found1.IsSome

  let containsPairOfLetters2 (s:string) : bool =
    // printfn "scanning: %s" s

    let found =
      s
      |> Seq.windowed 2
      |> Seq.mapi (fun i cc -> (i,cc))
      |> Seq.filter (fun (i,cc) -> cc.Length = 2)
      |> Seq.tryFind (fun (i,cc) ->
        let pair = cc |> String.Concat
        let rest = s.Substring(i+2)
        // printfn "looking for %s in %s" pair rest
        rest.Length >= 2 && rest.Contains(pair)
      )

    found.IsSome

  // It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
  let atLeastOneRepeatedLetter (s:string) : bool =
    let found =
      s
      |> Seq.windowed 3
      |> Seq.tryFind (fun w -> w.[0] = w.[2])

    found.IsSome

  let isNice (input:string) : bool =
    let a = (containsAtLeastNVowels 3 input) 
    let b = (atLeastOneDuplicatedLetter 2 input)
    let c = (doesNotMatch ["ab"; "cd"; "pq"; "xy"] input)

    // printfn "%s %b %b %b" input a b c

    a && b && c

  let isNice2 (input:string) : bool =
    let a = (containsPairOfLetters2 input)
    let b = (atLeastOneRepeatedLetter input)

    a && b
