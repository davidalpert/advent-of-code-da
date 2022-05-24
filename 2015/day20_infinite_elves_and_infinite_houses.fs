namespace AdventOfCode

module InfiniteElvesandInfiniteHouses =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Array.ofSeq


    let divisorsOf (n: int) =
        seq {
            for i in 1..n do
                if i * i <= n && n % i = 0 then
                    if n / i = i then
                        yield i
                    else
                        yield i
                        yield n / i
        }
    // |> Seq.sort

    // begin: https://www.bartwolff.com/Blog/2013/04/04/project-euler-problem-21
    let Divisors n =
        [ 1 .. (n / 2) + 1 ]
        |> List.filter (fun x -> n % x = 0)

    let SumDivList n = Divisors n |> List.sum

    let DivMap n =
        if n = 1 then
            [| false; true |]
        else
            let m = Array.create ((n / 2) + 1) false
            m.[1] <- true

            for i in 2 .. m.Length - 1 do
                if m.[i] = false && n % i = 0 then
                    m.[i] <- true
                    m.[n / i] <- true

            m

    let SumDivMap n =
        match n with
        | 1 -> 1
        | _ ->
            let mutable s = 0
            let m = DivMap n

            for i in 1 .. m.Length - 1 do
                if m.[i] = true then s <- s + i

            // had to add n as it seemed to be missing
            s + n
    // end: https://www.bartwolff.com/Blog/2013/04/04/project-euler-problem-21

    let numPresentsDeliveredToHouseN (n: int) = (SumDivMap n) * 10
    // n
    // |> divisorsOf
    // |> Seq.map (fun i -> i * 10)
    // |> Seq.sum

    let houseNumbersWhichGetAtLeastNPresents (n: int) =
        seq {
            for i in 1 .. (n / 10) do
                let p = numPresentsDeliveredToHouseN i
                // printfn "considering '%d' (%d presents)" i p
                if i % 1000 = 0 then printf "."

                if p >= n then yield i
        }
