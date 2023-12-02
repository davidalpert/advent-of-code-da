namespace AdventOfCode

module cubeconundrum =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    type CubeColor =
        | Red
        | Blue
        | Green

    type CubeSet =
        { n: int; color: CubeColor }

    type Handful =
        { red: int; blue: int; green: int }
        
        member this.possibleGiven (h:Handful) =
            this.red <= h.red &&
            this.blue <= h.blue &&
            this.green <= h.green

        static member fromCubeSets (s:CubeSet list) =
           let red = s |> List.tryFind (fun c -> c.color = Red) 
           let green = s |> List.tryFind (fun c -> c.color = Green) 
           let blue = s |> List.tryFind (fun c -> c.color = Blue)
           let n2i (n: CubeSet option) =
               match n with
               | Some(s) -> s.n
               | None -> 0
           { red = n2i red; blue = n2i blue; green = n2i green }
           
    type Game =
        {
            ID: int
            reveals: Handful array
        }
        static member fromIdAndReveals id rev =
            { ID = id; reveals = rev |> Array.ofSeq }

        member this.minimumNumberOfCubes =
            {
                red = this.reveals |> Seq.map (fun h -> h.red) |> Seq.max;
                blue = this.reveals |> Seq.map (fun h -> h.blue) |> Seq.max;
                green = this.reveals |> Seq.map (fun h -> h.green) |> Seq.max;
            }

        // The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together.
        member this.power =
            let min = this.minimumNumberOfCubes
            min.red * min.green * min.blue

    module parser =
        let ws = spaces

        let pColor colorName color =
            %% ws -- +.pint32 -- ws -? pstring colorName
            -|> fun n -> { n = n; color = color  }

        let pSet =
           %[ 
                pColor "blue" Blue;
                pColor "red" Red;
                pColor "green" Green;
           ]

        let pHandful =
           %% ws -- +.(sepBy1 pSet %", ")
           -|> Handful.fromCubeSets

        let pGame =
            %% ws -- %"Game" -- ws -- +.pint32 -- %":"
            -- ws -- +.(sepBy1 pHandful %";") -- ws
            -|> Game.fromIdAndReveals

        let pRecord =
            %% ws -- +.(pGame * qty.[1..])
            -|> Array.ofSeq

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse pRecord input

    let possibleGamesGiven (h:Handful) (input: string) =
        input
        |> parser.parseInput
        |> Seq.where (fun g ->
                g.reveals |> Seq.forall (fun r -> r.possibleGiven h)
            )

    let sumOfIDsOfPossibleGamesGiven (h: Handful) (input: string) =
        input
        |> possibleGamesGiven h
        |> Seq.sumBy (fun g -> g.ID)

    let sumOfPowers (input:string) =
        input
        |> parser.parseInput
        |> Array.sumBy (fun g -> g.power)