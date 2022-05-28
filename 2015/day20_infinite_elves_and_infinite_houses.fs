namespace AdventOfCode

module InfiniteElvesandInfiniteHouses =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    // the second version of https://www.geeksforgeeks.org/find-all-divisors-of-a-natural-number-set-2/
    // in C# is a solution in O(sqrt(n)) Time and O(1) Space; here is an F# way to express this sequence
    let divisorsAsSeq (n: int) : seq<int> =
        match n with
        | 1 -> 1 |> Seq.singleton // 1 is a special case
        | _ ->
            let s = Math.Sqrt(n) |> int

            seq {
                yield!
                    seq {
                        for i in 1..1..s do
                            if n % i = 0 then i
                    }

                yield!
                    seq {
                        for i in s .. -1 .. 1 do
                            if n % i = 0 then n / i
                    }
            }
            |> Seq.distinct

    let sumDivSeq (n: int) = n |> divisorsAsSeq |> Seq.sum

    let numPresentsDeliveredToHouseN (n: int) = (sumDivSeq n) * 10

    let houseNumbersWhichGetAtLeastNPresents (n: int) =
        seq {
            for i in 1 .. (n / 10) do
                let p = numPresentsDeliveredToHouseN i
                // printfn "considering '%d' (%d presents)" i p
                // if i % 1000 = 0 then printf "."

                if p >= n then yield i
        }

    let sumDivSeqWithLimit (l: int) (n: int) =
        n
        |> divisorsAsSeq
        |> Seq.filter (fun x -> n / x <= 50)
        |> Seq.sum

    let numPresentsDeliveredToHouseNWithLimit (l: int) (n: int) = (sumDivSeqWithLimit l n) * 11

    // // I was close; this comes from https://www.reddit.com/r/adventofcode/comments/3xjpp2/comment/cy59ygt/?utm_source=share&utm_medium=web2x&context=3
    // let factors number =
    //     seq {
    //         for divisor in 1 .. (float >> sqrt >> int) number do
    //             if number % divisor = 0 then
    //                 yield (number, divisor)
    //                 yield (number, number / divisor)
    //     }

    // let find filt pres n =
    //     Seq.initInfinite (fun i ->
    //         (factors i)
    //         |> Seq.distinctBy snd
    //         |> Seq.filter filt
    //         |> Seq.sumBy snd
    //         |> (*) pres)
    //     |> Seq.findIndex (fun sum -> sum >= n)

    // let findPart2UsingSolutionFromReddit n =
    //     (find (fun (i, fact) -> i / fact <= 50) 11 n)

    let houseNumbersWhichGetAtLeastNPresentsWithElvesStoppingAfter50Houses (n: int) =

        // findPart2UsingSolutionFromReddit n
        (seq {
            for i in 1 .. (n / 11) do
                yield numPresentsDeliveredToHouseNWithLimit 50 i
         }
         |> Seq.findIndex (fun s -> s >= n))
        + 1
