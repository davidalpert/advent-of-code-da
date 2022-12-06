namespace AdventOfCode

module TuningTrouble =

    let firstMarkerAfterNDistinctCharacters n (buffer: string) =
        let startOfPacketMarkerIndex =
            buffer.Trim().ToCharArray()
            |> Seq.windowed n
            |> Seq.findIndex (fun window ->
                    // printfn $"inspecting: %s{window |> String}"
                    (window |> Array.distinct |> Array.length) = n
                )
            
        // printfn "-----"
        startOfPacketMarkerIndex + n
        
    let packetStartsAt buffer =
        firstMarkerAfterNDistinctCharacters 4 buffer
        
    let messageStartsAt buffer =
        firstMarkerAfterNDistinctCharacters 14 buffer
        