namespace AdventOfCode

open FParsec

module day03_Gear_Ratios =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    /// The engine schematic (your puzzle input) consists of a visual
    /// representation of the engine. There are lots of numbers and
    /// symbols you don't really understand, but apparently any number
    /// adjacent to a symbol, even diagonally, is a "part number" and
    /// should be included in your sum.
    /// (Periods (.) do not count as a symbol.)

    type Pos =
        { y: int64; x: int64; }
        
        static member fromPosition (p:Position) =
            { y = p.Line; x = p.Column }
    
    type Number =
        {
            p: Pos;
            len: int64;
            v: int;
        }
        member this.adjacentPositions =
            seq {
               for line in (this.p.y - 1L) .. (this.p.y  + 1L) do
                   for col in (this.p.x- 1L) .. (this.p.x + this.len) do
                       yield if line = this.p.y && this.p.x <= col && col < this.p.x + this.len then 
                                None
                             else
                                Some({ y = line; x = col; })
            }
            |> Seq.choose id
            |> Array.ofSeq

    type Symbol =
        {
            p: Pos;
            c: char
        }

    type Schematic =
        {
            numbers: Number array
            symbols: Symbol array
        }
        member this.symbolPositions = this.symbols |> Array.map (fun s -> s.p)
        member this.partNumbers =
            this.numbers |> Array.where (fun n ->
                    n.adjacentPositions |> Array.exists (fun p ->
                        this.symbolPositions |> Array.contains p
                    )
                )
        member this.sumOfPartNumbers = this.partNumbers |> Array.sumBy (fun p -> p.v)

    let sumOfPartNumbers (s:Schematic) =
        s.sumOfPartNumbers

    type Atom =
    | ParsedNumber of Number
    | ParsedSymbol of Symbol
    | ParsedPeriod
    
    module parser =
        let ws = spaces
        let ch = pchar

        let pNumber =
            %% ws -- +.(getPosition) -- +.pint32
            -|> fun p v -> ParsedNumber({
                p = p |> Pos.fromPosition
                len = v.ToString().Length; 
                v = v;
            })
        
        let pPeriod =
            %% ws -- ch '.' -|> ParsedPeriod
        
        let pSymbol =
            %% ws -- +.(getPosition) -- +.anyChar
            -|> fun p c -> ParsedSymbol({
                p = p |> Pos.fromPosition;
                c = c;
            })

        let pSchematic =
            %% ws
            -- +.(%[
                      attempt pNumber;
                      attempt pPeriod;
                      attempt pSymbol;
                   ] * qty[1..])
            -- ws
            -|> fun atoms ->
                let nn =
                    atoms
                    |> Seq.choose (fun a -> match a with | ParsedNumber(n) -> Some(n) | _ -> None )
                    |> Array.ofSeq
                    
                let ss =
                    atoms
                    |> Seq.choose (fun a -> match a with | ParsedSymbol(n) -> Some(n) | _ -> None )
                    |> Array.ofSeq
                    
                {
                    numbers = nn
                    symbols = ss; 
                }

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse pSchematic input
