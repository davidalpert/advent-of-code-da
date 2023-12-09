namespace AdventOfCode

module day08_Haunted_Wasteland =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    type Instruction =
    | R
    | L

    type Node =
        {
            name: string
            neighbors: string * string
        }
        member this.nextElement (i:Instruction) =
            match i with
            | L -> this.neighbors |> fst
            | R -> this.neighbors |> snd
        member this.str = $"%s{this.name}"

    type Step =
        {
            number:int
            node:Node
            instruction:Instruction
        }
    type Map =
        {
            instruction_seed: Instruction array
            nodes_by_name: Map<string, Node>
        }
        member this.instructionForStep (i:int) =
            this.instruction_seed[i % this.instruction_seed.Length]

    let followFrom startingNodeName isEndingName (m:Map) =
        let startNode = m.nodes_by_name[startingNodeName]
        (0,  startNode)
        |> Seq.unfold (fun (s,n) ->
            let thisStep = (s,n)
            match n.name with
            | s when s |> isEndingName -> None
            | _     ->
                let nextNodeName =
                    m.instructionForStep s |> m.nodes_by_name[n.name].nextElement
                let nextNode = m.nodes_by_name[nextNodeName]
                let nextStep = (s+1, nextNode)
                Some(thisStep, nextStep)
        )
    
    let follow (m:Map) =
        let isEndingName (s:String) = s = "ZZZ"
        followFrom "AAA" isEndingName m
        
    let part2FollowByMath (m:Map) =
        let isStartingName (n:string) = n.EndsWith("A")
        let isEndingName (n:string) = n.EndsWith("Z")
        let isEndingNode (n:Node) = n.name |> isEndingName

        let startingNodes =
            m.nodes_by_name.Keys
            |> Seq.where isStartingName
            |> Seq.map (fun n -> m.nodes_by_name[n])
            |> Array.ofSeq

        let cycles =
            startingNodes
            |> Array.map (fun n -> m |> followFrom n.name isEndingName)

        // to look at one at a time
        cycles |> Array.skip 0 |> Array.head
        // cycles
        // let lengthOfCycles = cycles |> Array.map (fun n -> n |> Seq.length)
        // lengthOfCycles |> lcmByVennDiagram
        
    let part2Follow (m:Map) =
        let isStartingName (n:string) = n.EndsWith("A")
        let isEndingName (n:string) = n.EndsWith("Z")
        let isEndingNode (n:Node) = n.name |> isEndingName

        let startingNodes =
            m.nodes_by_name.Keys
            |> Seq.where isStartingName
            |> Seq.map (fun n -> m.nodes_by_name[n])
            |> Array.ofSeq

        (0, startingNodes)
        |> Seq.unfold (fun (s,nn) ->
            let thisStep = (s,nn)
            let allFinished = Array.TrueForAll(nn, isEndingNode)
            if allFinished then
                None
            else
                let nextNodes = 
                    nn
                    |> Array.map (fun n ->
                        let nextNodeName =
                            m.instructionForStep s |> m.nodes_by_name[n.name].nextElement
                        let nextNode = m.nodes_by_name[nextNodeName]
                        nextNode
                    )
                let nextStep = (s + 1, nextNodes)
                Some(thisStep, nextStep)
        )

    module parser =
        let ws = spaces
        let ch = pchar

        let pL = %% %'L' -|> L 
        let pR = %% %'R' -|> R

        let pInstruction =
            attempt %[pL; pR]
        
        let pNodeName = anyString 3

        let pNode =
            %% ws -? +.(pNodeName) -- %" = " -- %'(' -- +.pNodeName -- %", " -- +.pNodeName -- %')'
            -|> fun a b c -> { name = a; neighbors = (b,c) }

        let pMap =
            %% ws -- +.(pInstruction * qty[1..])
            -- ws -- +.(pNode * qty[1..])
            -|> (fun ii nn ->
                {
                    instruction_seed = ii |> Array.ofSeq
                    nodes_by_name = nn |> Seq.map (fun n -> n.name, n) |> Map
                }
            )

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse pMap input

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Array.ofSeq
