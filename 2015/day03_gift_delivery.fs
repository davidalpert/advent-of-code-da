namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils
open System.Collections.Generic

module GiftDelivery =

  type Location = {
    x: int
    y: int
  }

  let deliveryInstructionsToGiftsPerLocation (input:string) =
    let dict = new Dictionary<Location, int>()
    let initialLocation = { x = 0; y = 0 }
    dict.Add(initialLocation, 1)

    let folder (l:Location) (c:char) =
      let nextLocation = match c with
                         | '^' -> { l with y = l.y + 1 }
                         | 'v' -> { l with y = l.y - 1 }
                         | '>' -> { l with x = l.x + 1 }
                         | '<' -> { l with x = l.x - 1 }
                         | _ -> l

      if nextLocation <> l then
        match dict.ContainsKey(nextLocation) with
        | true -> dict.[nextLocation] <- dict[nextLocation] + 1
        | false -> dict.Add(nextLocation, 1)

      nextLocation

    let finalLocation =
      input
      |> Seq.fold folder initialLocation
    
    dict