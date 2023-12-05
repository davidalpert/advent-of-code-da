namespace AdventOfCode

module day05_If_You_Give_A_Seed_A_Fertilizer =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    type Seed =
        {
            number: int64
            soil: int64
            fertilizer: int64
            water: int64
            light: int64
            temperature: int64
            humidity: int64
            location: int64
        }

        static member zero =
            {
              number = 0L;
              soil = 0L;
              fertilizer = 0L;
              water = 0L;
              light = 0L;
              temperature = 0L;
              humidity = 0L;
              location = 0L;
            }

        static member build n =
            { Seed.zero with number = n }

    type MapType =
        | SeedToSoilMap
        | SoilToFertilizerMap
        | FertilizerToWaterMap
        | WaterToLightMap
        | LightToTemperatureMap
        | TemperatureToHumidityMap
        | HumidityToLocationMap
        | UnknownMap
        static member fromString s =
            match s with
            | "seed-to-soil" -> SeedToSoilMap
            | "soil-to-fertilizer" -> SoilToFertilizerMap
            | "fertilizer-to-water" -> FertilizerToWaterMap
            | "water-to-light" -> WaterToLightMap
            | "light-to-temperature" -> LightToTemperatureMap
            | "temperature-to-humidity" -> TemperatureToHumidityMap
            | "humidity-to-location" -> HumidityToLocationMap
            | _ -> UnknownMap

    type ConversionMapping =
        {
            destinationRangeStart: int64
            sourceRangeStart: int64
            rangeLength: int64
        }
        member this.sourceRangeEndInclusive =
            this.sourceRangeStart + this.rangeLength - 1L

        member this.inSourceRange n =
            this.sourceRangeStart <= n && n <= this.sourceRangeEndInclusive
            
        member this.mapToDestination n =
            let offset = n - this.sourceRangeStart
            this.destinationRangeStart + offset

    type ConversionMap =
        {
            name: MapType
            mappings: ConversionMapping array
        }
        static member build name mappings =
            {
                name = MapType.fromString name
                mappings = mappings |> Array.ofSeq 
            }

        member this.applyMapping n =
            let mm =
                this.mappings
                |> Array.tryFind (fun m -> n |> m.inSourceRange)

            match mm with
            | Some(m) -> m.mapToDestination n
            | None    -> n

    type Almanac =
        {
            seeds: Seed array
            mappingBlocks: ConversionMap array
        }
        static member build seeds mappings =
            {
                seeds = seeds
                mappingBlocks = mappings
            }

        member this.calculateLocations =
            let folder (state:Seed array) (block:ConversionMap) =
                match block.name with
                | SeedToSoilMap            -> state |> Array.map(fun s -> { s with soil = block.applyMapping s.number })
                | SoilToFertilizerMap      -> state |> Array.map(fun s -> { s with fertilizer = block.applyMapping s.soil })
                | FertilizerToWaterMap     -> state |> Array.map(fun s -> { s with water = block.applyMapping s.fertilizer })
                | WaterToLightMap          -> state |> Array.map(fun s -> { s with light = block.applyMapping s.water })
                | LightToTemperatureMap    -> state |> Array.map(fun s -> { s with temperature = block.applyMapping s.light })
                | TemperatureToHumidityMap -> state |> Array.map(fun s -> { s with humidity = block.applyMapping s.temperature })
                | HumidityToLocationMap    -> state |> Array.map(fun s -> { s with location = block.applyMapping s.humidity })
                | UnknownMap               -> state

            this.mappingBlocks
            |> Array.fold folder this.seeds

        member this.lowestLocation =
            this.calculateLocations
            |> Array.minBy (fun s -> s.location)
            |> (fun s -> s.location)

        member this.expandedSeedList =
            this.seeds
            |> Array.chunkBySize 2
            |> Seq.collect (fun pair ->
                let rangeStart = pair[0].number
                let rangeLength = pair[1].number
                let rangeEndInclusive = rangeStart + rangeLength - 1L
                seq { rangeStart .. rangeEndInclusive}
                |> Seq.map (fun n -> {Seed.zero with number = n })
            )
            |> Array.ofSeq
            
        member this.part2Calculations =
            let folder (state:Seed array) (block:ConversionMap) =
                match block.name with
                | SeedToSoilMap            -> state |> Array.map(fun s -> { s with soil = block.applyMapping s.number })
                | SoilToFertilizerMap      -> state |> Array.map(fun s -> { s with fertilizer = block.applyMapping s.soil })
                | FertilizerToWaterMap     -> state |> Array.map(fun s -> { s with water = block.applyMapping s.fertilizer })
                | WaterToLightMap          -> state |> Array.map(fun s -> { s with light = block.applyMapping s.water })
                | LightToTemperatureMap    -> state |> Array.map(fun s -> { s with temperature = block.applyMapping s.light })
                | TemperatureToHumidityMap -> state |> Array.map(fun s -> { s with humidity = block.applyMapping s.temperature })
                | HumidityToLocationMap    -> state |> Array.map(fun s -> { s with location = block.applyMapping s.humidity })
                | UnknownMap               -> state

            this.mappingBlocks
            |> Array.fold folder this.expandedSeedList

        member this.part2SeedWithLowestLocation =
            this.part2Calculations
            |> Array.minBy (fun s -> s.location)

        member this.part2LowestLocation =
            this.part2SeedWithLowestLocation |> (fun s -> s.location)

    module parser =
        let ws = spaces
        let ch = pchar

        let pSeeds =
            %% ws -- %"seeds:"
            -- +.(many1Till (ws >>. %p<int64>) newline)
            -|> fun ss -> ss |> Seq.map Seed.build |> Array.ofSeq
        
        let pMapping =
            %% ws -? +.(%p<int64>) -- ws -- +.(%p<int64>) --ws -- +.(%p<int64>)
            -|> fun d s r ->
                {
                    destinationRangeStart = d
                    sourceRangeStart = s
                    rangeLength = r 
                }
        
        let pMappingsBlock =
            %% ws -- +.(charsTillString " map:" true 50)
            -- +.(pMapping * qty[1..])
            -|> ConversionMap.build

        let pAlmanac =
            %% ws -- +.pSeeds
            -- ws -- +.(pMappingsBlock * qty[7])
            -|> Almanac.build

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parseInput (input:string) =
            mustParse pAlmanac input

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Array.ofSeq
