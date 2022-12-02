namespace AdventOfCode

open System.Xml.Serialization

module RockPaperScissors =

    open System
    open System.Collections.Generic
    open AdventOfCode.Input
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    
    type Shape =
    | Rock
    | Paper
    | Scissors
    
    // "The first column is what your opponent is going to play: A for Rock, B for Paper, and C for Scissors..."
    let opponentsShapeByLetter = Map [
        ("A", Rock);
        ("B", Paper);
        ("C", Scissors);
    ] 

    // The second column, you reason, must be what you should play in response: X for Rock, Y for Paper, and Z for Scissors.
    let myShapeByLetter = Map [
        ("X", Rock);
        ("Y", Paper);
        ("Z", Scissors);
    ]
    
    // score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors)
    let scoreForShape (s:Shape) =
        match s with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    
    type Outcome =
    | Win
    | Lose
    | Draw
    
    // Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. If both players choose the same shape, the round instead ends in a draw.
    let outcomeForRound ((me,opponent):(Shape * Shape)) =
        match (me,opponent) with
        
        | (Rock, Scissors) -> Win  | (Scissors, Rock) -> Lose
        | (Scissors, Paper) -> Win | (Paper, Scissors) -> Lose
        | (Paper, Rock) -> Win     | (Rock, Paper) -> Lose
        
        | (Rock, Rock) -> Draw
        | (Paper, Paper) -> Draw
        | (Scissors, Scissors) -> Draw
    
    // the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won)
    let scoreForOutcome (r:Outcome) =
        match r with
        | Lose -> 0
        | Draw -> 3
        | Win  -> 6
    
    let inputToTupledStrings (s:string) =
        s.Trim().Split("\n")
        |> Array.map (fun r -> r.Split(" ") |> Array.take 2)
        |> Array.map (fun rr -> (rr[0], rr[1]))
        
    let inputToRounds (s:string) =
        s
        |> inputToTupledStrings
        |> Array.map (fun (o,m) ->
                (
                    opponentsShapeByLetter[o],
                    myShapeByLetter[m]
                )
            )
       
    // The score for a single round is the score for the shape you selected ... plus the score for the outcome of the round.
    let myScoreForARound ((o,m):Shape * Shape) =
        // have to swap so the rules apply from my perspective
        (m |> scoreForShape) + ((m,o) |> outcomeForRound |> scoreForOutcome)
        
    // Part 2 - "Anyway, the second column says how the round needs to end: X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win. Good luck!"
    let desiredOutcomeByLetter = Map [
        ("X", Lose);
        ("Y", Draw);
        ("Z", Win);
    ]
    
    let whatShapeDoIPlay (opponentsMove:Shape) (desiredOutcome:Outcome) : Shape =
        match (opponentsMove, desiredOutcome) with
        | (Rock, Win) -> Paper // I cover the rock
        | (Rock, Lose) -> Scissors // rock smashes my scissors
        | (Rock, Draw) -> Rock
        
        | (Paper, Win) -> Scissors // I cut the paper
        | (Paper, Lose) -> Rock // My rock gets covered
        | (Paper, Draw) -> Paper
        
        | (Scissors, Win) -> Rock // I smash the scissors
        | (Scissors, Lose) -> Paper // I get cut
        | (Scissors, Draw) -> Scissors

    let inputToRoundsPart2 (s:string) =
        s
        |> inputToTupledStrings
        |> Array.map (fun (opponentsMove, desiredOutcome) ->
              opponentsShapeByLetter[opponentsMove], desiredOutcomeByLetter[desiredOutcome]
            )
        |> Array.map (fun (opponentsShape, desiredOutcome) ->
                opponentsShape, (whatShapeDoIPlay opponentsShape desiredOutcome)
            )
