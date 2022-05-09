namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module NotQuiteLisp =
  let nextFloor (currentFloor:int) (c:char) =
    match c with
    | '(' -> currentFloor + 1
    | ')' -> currentFloor - 1
    | _   -> currentFloor

  let followDirections (directions:string) =
    directions
    |> Seq.fold nextFloor 0

  let positionWhichEntersBasement (directions:string) =
    // state is position,currentFloor
    ((0,0), directions)
    ||> Seq.scan (fun (currentPosition,currentFloor) c ->
        (currentPosition + 1, (nextFloor currentFloor c))
      )
    |> Seq.takeWhile (fun pair -> pair |> snd >= -1)
    |> Seq.last
    |> fst
