namespace AdventOfCode

module Day19 =

    open System
    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.MedicineforRudolph
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
H => HO
H => OH
O => HH
"""

    let exampleInput2 =
        """
e => H
e => O
H => HO
H => OH
O => HH
"""

    let puzzleInput =
        """
Al => ThF
Al => ThRnFAr
B => BCa
B => TiB
B => TiRnFAr
Ca => CaCa
Ca => PB
Ca => PRnFAr
Ca => SiRnFYFAr
Ca => SiRnMgAr
Ca => SiTh
F => CaF
F => PMg
F => SiAl
H => CRnAlAr
H => CRnFYFYFAr
H => CRnFYMgAr
H => CRnMgYFAr
H => HCa
H => NRnFYFAr
H => NRnMgAr
H => NTh
H => OB
H => ORnFAr
Mg => BF
Mg => TiMg
N => CRnFAr
N => HSi
O => CRnFYFAr
O => CRnMgAr
O => HP
O => NRnFAr
O => OTi
P => CaP
P => PTi
P => SiRnFAr
Si => CaSi
Th => ThCa
Ti => BP
Ti => TiTi
e => HF
e => NAl
e => OMg

CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl
"""

    [<Theory>]
    [<InlineData("HOH", 4)>]
    [<InlineData("HOHOHO", 7)>]
    let ``2015 - Day 19 - part 1 - calibration example`` (s: string, expected: int) =
        (sprintf "%s\n\n%s" exampleInput s)
        |> fromInput
        |> (fun m -> m.calibrationSet)
        // |> mapRenderEach
        |> Seq.length
        |> should equal expected

    [<Fact>]
    let ``2015 - Day 19 - part 1`` () =
        puzzleInput
        |> fromInput
        |> (fun m -> m.calibrationSet)
        // |> mapRenderEach
        |> Seq.length
        |> should equal 518

    [<Theory>]
    [<InlineData("HOH", 3)>]
    [<InlineData("HOHOHO", 6)>]
    let ``2015 - Day 19 - part 2 - construction example - width-first`` (s: string, expected: int) =
        (sprintf "%s\n\n%s" exampleInput2 s)
        |> fromInput
        |> (fun m -> m.cheapestConstructionSteps m.calibrationString)
        // |> mapRenderEach
        |> Seq.length
        |> should equal expected

    // [<Fact>] // width-first e -> molecule: sloooooooooow
    let ``2015 - Day 19 - part 2`` () =
        puzzleInput
        |> fromInput
        |> (fun m -> m.cheapestConstructionSteps m.calibrationString)
        // |> mapRenderEach
        |> Seq.length
        |> should equal -1

    [<Fact>]
    let ``2015 - Day 19 - part 2 - greedy replacement`` () =
        let reverseSequence =
            puzzleInput
            |> fromInput
            |> (fun m -> m.cheapestConstructionStepsTopDownGreedy m.calibrationString)

        // reverseSequence |> Seq.rev |> Seq.iteri (fun i (a, b, p) -> printfn "step '%d': %s" i p)

        reverseSequence |> Seq.length |> should equal 200
