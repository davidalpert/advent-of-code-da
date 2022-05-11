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
  
  type SantaTracker = {
    Santa : Location
    RoboDog : Location
  }

  let deliveryInstructionsToGiftsPerLocationWithRoboDog (input:string) =
    let initialLocation = { x = 0; y = 0 }
    let tracker = {
      Santa = initialLocation;
      RoboDog = initialLocation;
    }
    let dict = new Dictionary<Location, int>()

    let visit (l:Location) =
      match dict.ContainsKey(l) with
      | true -> dict.[l] <- dict[l] + 1
      | false -> dict.Add(l, 1)

    visit (tracker.Santa)
    visit (tracker.RoboDog)
    // Santa and Robo-Santa start at the same location (delivering two presents to the same starting house)

    let nextLocation (l:Location) (c:char) =
      match c with
      | '^' -> { l with y = l.y + 1 }
      | 'v' -> { l with y = l.y - 1 }
      | '>' -> { l with x = l.x + 1 }
      | '<' -> { l with x = l.x - 1 }
      | _ -> l

    let folder (tracker:SantaTracker, stepNumber:int) (c:char) =

      let santaMovesNext = stepNumber % 2 = 0

      let nextTracker =
        match santaMovesNext with
        | true -> { tracker with Santa = (c |> nextLocation(tracker.Santa)) }
        | false -> { tracker with RoboDog = (c |> nextLocation(tracker.RoboDog)) }
        
      if nextTracker.Santa <> tracker.Santa then
        visit (nextTracker.Santa)

      if nextTracker.RoboDog <> tracker.RoboDog then
        visit (nextTracker.RoboDog)

      (nextTracker, stepNumber + 1)

    let finalLocation =
      input
      |> Seq.fold folder (tracker, 0)
    
    dict