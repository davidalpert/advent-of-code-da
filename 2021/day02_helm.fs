namespace AdventOfCode

open AdventOfCode.Input

module Helm =

  let submarineInstructionsFromInput (input: string) =
    splitToTrimmedLines input
    |> Seq.map (fun s -> s.Split " ")
    |> Seq.map List.ofArray

  type SubmarinePosition =
    { horizontal: int
      depth: int
      aim: int }
    member x.displacement = x.horizontal * x.depth

  let initialPosition = { horizontal = 0; depth = 0; aim = 0 }

  let navigateSubmarine (startingPosition: SubmarinePosition) (instructions: string list) =
    match instructions with
    | "forward" :: [ x ] -> { startingPosition with horizontal = startingPosition.horizontal + (x |> int) }
    | "up" :: [ x ] -> { startingPosition with depth = startingPosition.depth - (x |> int) }
    | "down" :: [ x ] -> { startingPosition with depth = startingPosition.depth + (x |> int) }
    | _ -> startingPosition

  let navigateSubmarineWithAim (startingPosition: SubmarinePosition) (instructions: string list) =
    match instructions with
    | "down" :: [ x ] -> { startingPosition with aim = startingPosition.aim + (x |> int) }
    | "up" :: [ x ] -> { startingPosition with aim = startingPosition.aim - (x |> int) }
    | "forward" :: [ x ] ->
        { startingPosition with
            horizontal = startingPosition.horizontal + (x |> int)
            depth =
                startingPosition.depth
                + (startingPosition.aim * (x |> int)) }
    | _ -> startingPosition
