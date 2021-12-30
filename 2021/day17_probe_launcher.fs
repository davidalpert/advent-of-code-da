namespace AdventOfCode

module ProbeLauncher =

  type Velocity = { x : int ; y : int }

  type Position = { x : int ; y : int }
  with
    member p.isPast (area:TargetArea) =
      p.x > area.p2.x && p.y < area.p1.y

  and TargetArea = {
    p1: Position
    p2: Position
  }
  with
    member area.contains (p:Position) =
      area.p1.x <= p.x && p.x <= area.p2.x &&
      area.p1.y <= p.y && p.y <= area.p2.y
    
module TargetAreaParser =
    open FParsec
    open FParsec.Pipes
    open ProbeLauncher

    let sequence =
      %% +.p<int> -- ".." -- +.p<int>
      -|> fun a b -> (a,b)

    let pTargetArea =
      %% "target area: x=" -- +.sequence -- ", y=" -- +.sequence
      -|> fun xs ys -> {
        p1 = { x = fst xs; y = fst ys };
        p2 = { x = snd xs; y = snd ys }
      }

    let parse (input:string) =
      match run pTargetArea input with
      | Success(r, _, _)   -> Result.Ok(r)
      | Failure(errorMsg, _, _) -> Result.Error(errorMsg)
