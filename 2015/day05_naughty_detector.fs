namespace AdventOfCode

open AdventOfCode.Input

module NaughtyDetector =

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

  let isNice (input:string) : bool =
    let a = (containsAtLeastNVowels 3 input) 
    let b = (atLeastOneDuplicatedLetter 2 input)
    let c = (doesNotMatch ["ab"; "cd"; "pq"; "xy"] input)

    // printfn "%s %b %b %b" input a b c

    a && b && c

