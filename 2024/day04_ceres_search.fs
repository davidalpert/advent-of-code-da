namespace AdventOfCode

module day04_Ceres_Search =

    open System
    open System.Collections.Generic
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    module part1 =
        let needle = [|'X';'M';'A';'S'|]

        let surroundingPositions (x,y) =
            [|
                [|(x,y);(x-1,y-1);(x-2,y-2);(x-3,y-3)|]; [|(x,y);(x,y-1);(x,y-2);(x,y-3)|]; [|(x,y);(x+1,y-1);(x+2,y-2);(x+3,y-3)|];
                [|(x,y);(x-1,y);(x-2,y);(x-3,y)|];                                          [|(x,y);(x+1,y);(x+2,y);(x+3,y)|];
                [|(x,y);(x-1,y+1);(x-2,y+2);(x-3,y+3)|]; [|(x,y);(x,y+1);(x,y+2);(x,y+3)|]; [|(x,y);(x+1,y+1);(x+2,y+2);(x+3,y+3)|];
            |]

        type Pos = { x:int; y:int }
        with
            override this.ToString() = $"(%d{this.x},%d{this.y})"
            
            static member fromTuple (p:int*int) = { x = fst p; y = snd p }

            member this.surroundingSearchPaths =
                surroundingPositions (this.x, this.y)
                |> Array.map (fun path -> path |> Array.map Pos.fromTuple)

        let findInstances (searchGrid:char array array) =
            let maxX = searchGrid.Length - 1
            let maxY = searchGrid.[0].Length - 1
            seq {
                for x in 0..maxX do
                    for y in 0..maxY do
                        yield { x = x; y = y }
            }
            |> Seq.collect (fun pos ->
                pos.surroundingSearchPaths
                |> Seq.map (fun path ->
                    let pathMatches = needle |> Array.mapi (fun i c ->
                        let p = path.[i]
                        if p.x < 0 || p.x > maxX || p.y < 0 || p.y > maxY then
                            None
                        else
                            if searchGrid.[p.x].[p.y] = c then
                                Some(c)
                            else
                                None
                    )
                    if pathMatches |> Array.forall Option.isSome then
                        Some(pos, path)
                    else
                        None
                )
                |> Seq.filter Option.isSome |> Array.ofSeq
            )
            |> Array.ofSeq

    module part2 =
        let needle = [|'M';'A';'S';'M';'S'|]
        let surroundingPositions (x,y) =
            [|
                // M.S
                // .A.
                // M.S
                [|(x,y);(x+1,y+1);(x+2,y+2);(x,y+2);(x+2,y)|]
                
                // S.M
                // .A.
                // S.M
                [|(x,y);(x-1,y-1);(x-2,y-2);(x,y-2);(x-2,y)|]
                
                // M.M
                // .A.
                // S.S
                [|(x,y);(x+1,y+1);(x+2,y+2);(x+2,y);(x,y+2)|]
                
                // S.S
                // .A.
                // M.M
                [|(x,y);(x+1,y-1);(x+2,y-2);(x+2,y);(x,y-2)|]
            |]

        type Pos = { x:int; y:int }
        with
            override this.ToString() = $"(%d{this.x},%d{this.y})"
            
            static member fromTuple (p:int*int) = { x = fst p; y = snd p }

            member this.surroundingSearchPaths =
                surroundingPositions (this.x, this.y)
                |> Array.map (fun path -> path |> Array.map Pos.fromTuple)

        let findInstances (searchGrid:char array array) =
            let maxX = searchGrid.Length - 1
            let maxY = searchGrid.[0].Length - 1
            seq {
                for x in 0..maxX do
                    for y in 0..maxY do
                        yield { x = x; y = y }
            }
            |> Seq.collect (fun pos ->
                pos.surroundingSearchPaths
                |> Seq.map (fun path ->
                    let pathMatches = needle |> Array.mapi (fun i c ->
                        let p = path.[i]
                        if p.x < 0 || p.x > maxX || p.y < 0 || p.y > maxY then
                            None
                        else
                            if searchGrid.[p.x].[p.y] = c then
                                Some(c)
                            else
                                None
                    )
                    if pathMatches |> Array.forall Option.isSome then
                        Some(pos, path)
                    else
                        None
                )
                |> Seq.filter Option.isSome |> Array.ofSeq
            )
            |> Array.ofSeq

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (_.ToCharArray())
        |> Array.ofSeq
