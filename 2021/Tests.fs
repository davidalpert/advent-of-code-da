module Tests

open AdventOfCode
open System
open Xunit
open FsUnit.Xunit

let splitToTrimmedLines (input:string) =
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())

let soundingsFromInput (input:string) =
    splitToTrimmedLines input
    |> Seq.map (fun s -> s |> int)

let submarineInstructionsFromInput (input:string) =
    splitToTrimmedLines input
    |> Seq.map (fun s -> s.Split " ")
    |> Seq.map List.ofArray

let diagnosticReportFromInput (input:string) =
    splitToTrimmedLines input
    |> Seq.map List.ofSeq
    |> List.ofSeq

let depthIncreased (prev,next) =
    next > prev

let numberOfTimesDepthIncreases (soundings:int seq) = 
    soundings
    |> Seq.pairwise
    |> Seq.where depthIncreased
    |> Seq.length

// https://rolfsuter.ch/code/f-transpose-a-list-of-lists-transpose-2d-matrix/
let rec transpose = function
    | (_::_)::_ as M -> 
        List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

[<Fact>]
let ``Day 01 - Part 1 - Example`` () =
    Input.day01sample
    |> soundingsFromInput
    |> numberOfTimesDepthIncreases
    |> should equal 7

[<Fact>]
let ``Day 01 - Part 1 - Calculation`` () =
    Input.day01
    |> soundingsFromInput
    |> numberOfTimesDepthIncreases
    |> should equal 1215

[<Fact>]
let ``Day 01 - Part 2 - Example`` () =
    Input.day01sample
    |> soundingsFromInput
    |> Seq.windowed 3
    |> Seq.map Seq.sum 
    |> numberOfTimesDepthIncreases
    |> should equal 5

[<Fact>]
let ``Day 01 - Part 2 - Calculation`` () =
    Input.day01
    |> soundingsFromInput
    |> Seq.windowed 3
    |> Seq.map Seq.sum 
    |> numberOfTimesDepthIncreases
    |> should equal 1150

type SubmarinePosition = {
    horizontal : int
    depth : int
    aim : int
}
with
    member x.displacement =
        x.horizontal * x.depth

let initialPosition = {
    horizontal = 0;
    depth = 0;
    aim = 0;
}

let navigateSubmarine (startingPosition:SubmarinePosition) (instructions:string list) =
    match instructions with
    | "forward" :: [x] -> { startingPosition with horizontal = startingPosition.horizontal + (x |> int) }
    | "up"      :: [x] -> { startingPosition with depth = startingPosition.depth - (x |> int) }
    | "down"    :: [x] -> { startingPosition with depth = startingPosition.depth + (x |> int) }
    | _ -> startingPosition

let navigateSubmarineWithAim (startingPosition:SubmarinePosition) (instructions:string list) =
    match instructions with
    | "down"    :: [x] -> { startingPosition with aim = startingPosition.aim + (x |> int) }
    | "up"      :: [x] -> { startingPosition with aim = startingPosition.aim - (x |> int) }
    | "forward" :: [x] -> {
            startingPosition with
                horizontal = startingPosition.horizontal + (x |> int);
                depth = startingPosition.depth + (startingPosition.aim * (x |> int));
        }
    | _ -> startingPosition

[<Fact>]
let ``Day 02 - Part 1 - Example`` () =
    Input.day02sample
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarine initialPosition
    |> (fun p -> p.displacement)
    |> should equal 150

[<Fact>]
let ``Day 02 - Part 1 - Calculation`` () =
    Input.day02
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarine initialPosition
    |> (fun p -> p.displacement)
    |> should equal 2_073_315

[<Fact>]
let ``Day 02 - Part 2 - Example`` () =
    Input.day02sample
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarineWithAim initialPosition
    |> (fun p -> p.displacement)
    |> should equal 900

[<Fact>]
let ``Day 02 - Part 2 - Calculation`` () =
    Input.day02
    |> submarineInstructionsFromInput
    |> Seq.fold navigateSubmarineWithAim initialPosition
    |> (fun p -> p.displacement)
    |> should equal 1_840_311_528

let mostCommonBit (col:char list) =
    col
    |> List.groupBy (fun c -> c)
    |> List.map (fun (bit, bits) -> (bit, bits |> List.length))
    |> List.sortByDescending (fun (b, n) -> n,(b|>int))
    |> List.head
    |> fst

let epsilonFromGamma (gamma:string) =
    gamma.ToCharArray()
    |> Array.map (fun bit -> match bit with | '0' -> '1' | '1' -> '0' | _ -> ' ')
    |> String

let powerConsumption report =
    let gammaBits =
        report
        |> transpose
        |> List.map mostCommonBit
        |> Array.ofList
        |> String

    let epsilonBits = gammaBits |> epsilonFromGamma

    Convert.ToInt64(gammaBits, 2) * Convert.ToInt64(epsilonBits, 2)

[<Fact>]
let ``Day 03 - Part 1 - Example`` () =
    Input.day03sample
    |> diagnosticReportFromInput
    |> powerConsumption
    |> should equal (198 |> int64)

[<Fact>]
let ``Day 03 - Part 1 - Calculation`` () =
    Input.day03data
    |> diagnosticReportFromInput
    |> powerConsumption
    |> should equal (1458194 |> int64)

let lifeSupportRating report =
    let bitValueOf (index:int) (row:char list) =
        row.[index]

    let filterByBitMatching (index:int) (list:char list list) =
        let mostCommonBits =
            list
            |> transpose
            |> List.map mostCommonBit
    
        let b = mostCommonBits.[index]

        list
        |> List.where (fun row -> if (row |> bitValueOf index) = b then true else false)

    let filterByBitNotMatching (index:int) (list:char list list) =
        let mostCommonBits =
            list
            |> transpose
            |> List.map mostCommonBit
    
        let b = mostCommonBits.[index]

        list
        |> List.where (fun row -> if (row |> bitValueOf index) = b then false else true)

    let rec loop (filter:int -> char list list -> char list list) (currentIndex:int) (remaining:char list list) =
        // printfn "loop %A %A" currentIndex remaining
        match remaining with
        | head :: [] -> Convert.ToInt64(head |> Array.ofList |> String, 2)
        | _ -> loop filter (currentIndex + 1) (filter currentIndex remaining)

    let o2rating =
        report
        |>  loop filterByBitMatching 0

    let c02rating =
        report
        |> loop filterByBitNotMatching 0

    // printfn "%A" (o2rating,c02rating)
    
    o2rating * c02rating

[<Fact>]
let ``Day 03 - Part 2 - Example`` () =
    Input.day03sample
    |> diagnosticReportFromInput
    |> lifeSupportRating
    |> should equal ( 230 |> int64 )

[<Fact>]
let ``Day 03 - Part 2 - Calculation`` () =
    Input.day03data
    |> diagnosticReportFromInput
    |> lifeSupportRating
    |> should equal ( 2829354 |> int64 )