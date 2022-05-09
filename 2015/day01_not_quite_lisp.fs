namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module NotQuiteLisp =
  let followDirections (directions:string) =
    directions
    |> Seq.fold (fun currentFloor direction ->
        match direction with
        | '(' -> currentFloor + 1
        | ')' -> currentFloor - 1
        | _   -> currentFloor
      ) 0
