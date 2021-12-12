namespace AdventOfCode

open AdventOfCode.Input

module DepthSounder =
    let soundingsFromInput (input: string) =
        splitToTrimmedLines input
        |> Seq.map (fun s -> s |> int)

    let depthIncreased (prev, next) = next > prev

    let numberOfTimesDepthIncreases (soundings: int seq) =
        soundings
        |> Seq.pairwise
        |> Seq.where depthIncreased
        |> Seq.length

    let numberOfTimesDepthIncreasesByWindow (windowSize: int) (soundings: int seq) =
        // split into windows
        soundings
        |> Seq.windowed windowSize
        // sum each window
        |> Seq.map Seq.sum
        |> numberOfTimesDepthIncreases
