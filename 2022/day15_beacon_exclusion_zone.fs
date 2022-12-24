namespace AdventOfCode

module BeaconExclusionZone =

    open AdventOfCode.Input
    open FParsec
    open FParsec.Pipes
    open AdventOfCode.utils

    type Point = {
        x: int64
        y: int64
    }
    
    // A taxicab geometry or a Manhattan geometry is a geometry
    // whose usual distance function or metric of Euclidean geometry
    // is replaced by a new metric in which the distance between
    // two points is the sum of the absolute differences of their
    // Cartesian coordinates.
    let manhattanDistanceFrom b a =
        abs (a.x - b.x) + abs (a.y - b.y)
    
    // manhattanDelta is the manhattanDistance broken down by component
    let manhattanDeltaFrom b a =
        {
            x = abs (a.x - b.x)
            y = abs (a.y - b.y)
        }
        
    type Sensor = {
        pos: Point
        nearestBeacon: Point
        delta: Point
        distance: int64
    }
    let position s = s.pos
    let closestBeacon s = s.nearestBeacon
    let manhattanDistance s = s.delta
    
    let farthestLeft ss = ss |> Array.minBy (fun s -> s.pos.x)
    let farthestRight ss = ss |> Array.maxBy (fun s -> s.pos.x)
    let mostShallow ss = ss |> Array.minBy (fun s -> s.pos.y)
    let mostDeep ss = ss |> Array.maxBy (fun s -> s.pos.y)
    
    let isAsCloseOrCloserThanBeacon p s =
        (s |> position |> manhattanDistanceFrom p) <= s.distance
        
    let fartherThanBeacon p s = not (isAsCloseOrCloserThanBeacon s p)
    
    type ScanReport = {
        sensors: Sensor[]
        beacons: Point[]
        min: Point
        max: Point
    }
    
    module parser =
        let pCoordinate =
            %% %"x=" -- +.pint64 -- %", y=" -- +.pint64 -|> fun x y -> {x = x; y = y}
            
        let pLine =
            %% %"Sensor at " -- +.pCoordinate -- %": closest beacon is at " -- +.pCoordinate
            -|> fun s b -> {
                pos = s
                nearestBeacon = b
                delta = s |> manhattanDeltaFrom b
                distance = s |> manhattanDistanceFrom b
            }
            
        let pLines =
            %% ws -- +.(pLine * (qty[1..] / newline)) -- ws -%> auto
            
        let parseScanReport (input: string) =
            let sensors = (mustParse pLines input) |> Array.ofSeq
            let beacons = sensors |> Array.map closestBeacon
            
            let leftMost = sensors |> farthestLeft
            let rightMost = sensors |> farthestRight
            let shallowMost = sensors |> mostShallow
            let deepMost = sensors |> mostDeep
            
            {
                sensors = sensors
                beacons = beacons
                min = {
                    x = leftMost.pos.x - leftMost.distance
                    y = shallowMost.pos.y - shallowMost.distance
                }
                max = {
                    x = rightMost.pos.x + rightMost.distance
                    y = deepMost.pos.y + deepMost.distance
                }
            }
        
    let canBeSeenByASensor (sensors: Sensor[]) (p: Point) =
        // p |> log id |> ignore
        sensors |> Array.tryFind (isAsCloseOrCloserThanBeacon p)
        
    let isABeacon (sensors: Sensor[]) (p: Point) =
        sensors |> Array.tryFind (fun s -> closestBeacon s = p)
        
    let part1_how_many_positions_cannot_contain_a_beacon y (input:string) =
        let report = input |> parser.parseScanReport
        
        seq { report.min.x .. report.max.x }
        |> Seq.filter (fun x ->
            let p = { x = x; y = y }
            (canBeSeenByASensor report.sensors p).IsSome && (isABeacon report.sensors p).IsNone)
        |> Seq.length
