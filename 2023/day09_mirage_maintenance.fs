namespace AdventOfCode

module day09_Mirage_Maintenance =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils

    let reductionIsComplete (vv:int32 array) = Array.TrueForAll(vv, (fun v -> v = 0))

    let rec nextValueFor (vv:int32 array) =
        let lastV = Array.last vv
        let diffs =
            vv |> Array.pairwise |> Array.map (fun (a,b) -> b - a)
        
        if diffs |> reductionIsComplete then
            lastV
        else
            lastV + (nextValueFor diffs)

    let nextValueForRev (vv:int32 array) =
        vv |> Array.rev |> nextValueFor

    type OASISReport =
        {
            historicalValuesByIndex: Map<int,int32 array>
        }
        member this.nextValueForIndex (index:int) =
            this.historicalValuesByIndex[index] |> nextValueFor

        member this.sumOfNextValues =
            this.historicalValuesByIndex.Values
            |> Seq.map nextValueFor
            |> Seq.reduce (+)

        member this.firstPrevValueForIndex (index: int) =
            this.historicalValuesByIndex[index] |> nextValueForRev

        member this.sumOfFirstPrevValues =
            this.historicalValuesByIndex.Values
            |> Seq.map nextValueForRev
            |> Seq.reduce (+)

    module parser =
        let parseInput (input:string) =
            {
                historicalValuesByIndex =  
                    input
                    |> splitToTrimmedLines
                    |> Seq.mapi (fun i line -> (i, line.Split(" ") |> Array.map int32))
                    |> Map
            }