namespace AdventOfCode

module ProboscideaVolcanium =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    type Valve = {
        name: string
        flowRate: int
        neighbors: string[]
    }
    
    let totalFlowFromOpeningValveAt v valveOpenedAtMinute =
        (valveOpenedAtMinute - 1) * v.flowRate
        
    let allFlowRates v =
        seq { 30 .. -1 .. 1 }
        |> Seq.map (fun i -> (i, (i |> totalFlowFromOpeningValveAt v)))
        |> Map.ofSeq
        
    let maxFlowRate m =
        m
        |> Map.values
        |> Seq.map (fun v -> v.flowRate)
        |> Seq.max
        
    let costOfEdge m a b =
        (maxFlowRate m) - m[a].flowRate
        
        
    module parser =
        let pValveName = anyString 2
        
        let pMaybePlural s = %[s+"s";s]
        
        let pValve =
            %% ws -- %"Valve " -- +.pValveName -- % " has flow rate=" -- +.pint32 -- ";" -- ws
            -- (pMaybePlural "tunnel") -- ws -- pMaybePlural "lead" -- ws -- "to" -- ws -- pMaybePlural "valve" -- ws -- +.(pValveName * (qty[1..] / ", "))
            -|> fun name rate neighbors -> name, { name = name; flowRate = rate; neighbors = neighbors |> Array.ofSeq }
            
        let pScanOutput =
            %% +.(pValve * qty[1..])
            -|> fun results -> results |> Map.ofSeq
        
        let parseInput (input:string) = mustParse pScanOutput input

    let fromInput (s: string) =
        s
        |> parser.parseInput
        |> log id
        |> Array.ofSeq
