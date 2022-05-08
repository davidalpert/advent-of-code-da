namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module ReportRepair =
    let integersFromInput (input: string) =
        splitToTrimmedLines input
        |> Seq.map (fun s -> s |> int)

    let chooserSumTo (n:int) (expenses: int list) =
        if expenses |> List.sum = n then
            Some(expenses)
        else
            None

    let entriesWhichSumByNTo (n:int) (s:int) (report: seq<int>) : (int list)  =
        report
        |> List.ofSeq
        |> comb n
        |> Seq.pick (chooserSumTo s)

    let multipleOfTuple (expenses: int list) =
        expenses |> List.fold (fun state expense -> state * expense) 1

    let multipleOfEntriesWhichSumByNTo (n:int) (s:int) (report: seq<int>) =
        report
        |> entriesWhichSumByNTo n s
        |> multipleOfTuple
