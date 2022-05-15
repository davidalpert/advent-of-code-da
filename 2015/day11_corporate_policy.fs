namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module CorporatePolicy =

    open System

    let nextChar (c: char) =
        match c with
        | 'z' -> 'a'
        | _ -> (char) (((int) c) + 1)

    // Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on. Increase the rightmost letter one step; if it was z, it wraps around to a, and repeat with the next letter to the left until one doesn't wrap around.
    let nextPassword (s: string) =
        let chars = s |> Array.ofSeq

        seq { 7..-1..0 }
        |> Seq.iter (fun i ->

            if i = 7 then
                chars.[i] <- nextChar chars.[i]
            elif chars.[i + 1] = 'a' then
                chars.[i] <- nextChar chars.[i])

        chars |> String

    // Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
    let includesIncreasingStraightOfMinNChars (n: int) (cc: char []) : bool =
        let sequenceStartsAt i =
            match i + n - 1 <= 7 with
            | false -> false // not enough room for a straight of n chars
            | true ->
                seq { i .. (i + n) }
                |> Seq.forall (fun j -> cc.[j + 1] = (char) (((int) cc.[j]) + 1))

        let found = seq { 0..7 } |> Seq.tryFind sequenceStartsAt

        found.IsSome

    // Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing.
    let containsChars (needles: seq<char>) (cc: char []) : bool =
        let found =
            cc
            |> Array.tryFind (fun c -> needles |> Seq.contains c)

        found.IsSome

    let doesNotContainChars (needles: seq<char>) (cc: char []) : bool = not (containsChars needles cc)

    // Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.
    let containsAtLeastTwoDifferentNonOverlappingPairs (cc: char []) : bool =

        let folder (state, lastMatchIndex: int option) i =
            match i with
            | 7 -> (state, None) // not finding a match which starts at the last character
            | _ ->
                if lastMatchIndex.IsSome
                   && lastMatchIndex.Value + 1 <= i then
                    (state, None)
                elif cc.[i] = cc.[i + 1] then
                    (state |> Seq.append (Seq.singleton i), Some(i))
                else
                    (state, None)

        let (nonOverlappingPairs, _) = seq { 0..7 } |> Seq.fold folder ([], None)

        (nonOverlappingPairs |> Seq.length) >= 2

    let isValid (cc: char []) : bool =
        [| cc |> includesIncreasingStraightOfMinNChars 3
           cc |> doesNotContainChars [ 'i'; 'o'; 'l' ]
           cc
           |> containsAtLeastTwoDifferentNonOverlappingPairs |]
        |> Array.forall (fun result -> result)
