namespace AdventOfCode

module BoilingBoulders =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames

    type Position = {
        x: int
        y: int
        z: int
    }

    let neighborMods =
        [|
            { x = -1; y =  0; z =  0 }; { x =  1; y =  0; z =  0 };
            { x =  0; y = -1; z =  0 }; { x =  0; y =  1; z =  0 };
            { x =  0; y =  0; z = -1 }; { x =  0; y =  0; z =  1 };
        |]

    let neighborsOf p =
        neighborMods
        |> Array.map (fun m -> { x = p.x + m.x; y = p.y + m.y; z = p.z + m.z })

    let isAdjacentTo p1 p2 =
        // same x and y but different z by 1
        (p1.x = p2.x && p1.y = p2.y && (abs (p1.z - p2.z)) = 1) ||
        // same y and z but different x by 1
        (p1.y = p2.y && p1.z = p2.z && (abs (p1.x - p2.x)) = 1) ||
        // same z and x but different y by 1
        (p1.z = p2.z && p1.x = p2.x && (abs (p1.y - p2.y)) = 1)

    let toPosition (input:string) =
        let components = input.Split(",") |> Array.map int32
        if components.Length < 3 then
            failwithf $"input string '$s{input}' needs at least three components"
        {
            x = components[0];
            y = components[1];
            z = components[2];
        }

    let positionAsString p = sprintf $"%d{p.x},%d{p.y},%d{p.z}"
    let positionsAsString s =
        s |> Seq.map (fun p -> sprintf $"%d{p.x},%d{p.y},%d{p.z}") |> joinBy "\n"

    let toSetOfPositions (input:string) =
        input.Trim()
        |> splitToTrimmedLines
        |> Seq.filter (fun s -> s <> "")
        |> Seq.map toPosition
        |> Set.ofSeq

    let contains s item =
        s |> Set.contains item

    let doesNotContain s item = not (contains s item)

    let emptyFacesImpliedBy occupied =
        let allDirectNeighbors =
            occupied |> Set.toSeq |> Seq.collect neighborsOf
            // |> log positionsAsString

        allDirectNeighbors
        |> Seq.filter (doesNotContain occupied)

    let part1_what_is_the_surface_area_of_the_lava_droplet_defined_by (input:string) =
        let occupied = input |> toSetOfPositions

        emptyFacesImpliedBy occupied
        |> Seq.length

    let xof p = p.x
    let yof p = p.y
    let zof p = p.z

    let extremesOf s =
        let positions = s |> Set.toArray

        let allX = positions |> Array.map xof
        let xmin = allX |> Array.min
        let xmax = allX |> Array.max

        let allY = positions |> Array.map yof
        let ymin = allY |> Array.min
        let ymax = allY |> Array.max

        let allZ = positions |> Array.map zof
        let zmin = allZ |> Array.min
        let zmax = allZ |> Array.max

        { x = xmin; y = ymin; z = zmin },{ x = xmax; y = ymax; z = zmax }

    let isSurrounded s (min,max) p =
        let x1 = seq { (p.x - 1) .. -1 .. min.x }
        let x2 = seq { (p.x + 1) .. max.x }
        let y1 = seq { (p.y - 1) .. -1 .. min.y }
        let y2 = seq { (p.y + 1) .. max.y }
        let z1 = seq { (p.z - 1) .. -1 .. min.z }
        let z2 = seq { (p.z + 1) .. max.z }

        x1 
        // let neighborsOfP = neighborsOf p |>

        false

    let isNotSurrounded s m p = not (isSurrounded s m p)

    let part2_what_is_the_external_surface_area_of_the_lava_droplet_defined_by (input:string) =
        let occupied = input |> toSetOfPositions
        let impliedEmptyFaces = emptyFacesImpliedBy occupied
        let mm = extremesOf occupied



        mm

    let fromInput (s: string) =
        s
        |> splitToTrimmedLines
        |> Seq.map (fun s ->
            // printfn "s: %s" s
            s)
        |> Array.ofSeq
