namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module ReportRepair =
    let integersFromInput (input: string) =
        splitToTrimmedLines input
        |> Seq.map (fun s -> s |> int)

    let chooserSumTo (n:int) (pair: int * int) =
        let (a,b) = pair
        // printfn "(%d + %d)" a b
        if a + b = n then
            Some(a,b)
        else
            None

    let entriesWhichSumTo (n:int) (report: seq<int>) : (int * int)  =
        report
        |> List.ofSeq
        |> comb 2
        |> Seq.map (fun ll -> (ll.[0], ll.[1]))
        |> Seq.pick (chooserSumTo n)

    let multipleOfTuple (a:int, b:int) =
        a * b

    let multipleOfEntriesWhichSumTo (n:int) (report: seq<int>) =
        report
        |> entriesWhichSumTo n
        |> multipleOfTuple
