namespace AdventOfCode

module JsonAbacus =

    open FParsec
    open AdventOfCode.Json.Ast
    open AdventOfCode.Json.Parser

    let sumOfAllNumbers (s: string) : float =

        let rec calculateSum (node: Json) =
            match node with
            | JNumber (f) -> f
            | JList (l) -> l |> List.sumBy calculateSum
            | JObject (m) ->
                m
                |> Seq.sumBy (fun pair -> calculateSum (pair.Value))
            | _ -> 0

        match parseJsonString s with
        | Success (v, _, _) -> calculateSum v
        | Failure (msg, err, _) -> raise (System.Exception(msg))

    let sumOfAllNumbers2 (s: string) : float =

        let isRed (node: Json) : bool =
            match node with
            | JObject (m) ->
                (m
                 |> Seq.tryFind (fun pair -> pair.Value = JString("red")))
                    .IsSome
            | _ -> false

        let rec calculateSum (node: Json) =
            if isRed node then
                0 |> float
            else
                match node with
                | JNumber (f) -> f
                | JList (l) -> l |> List.sumBy calculateSum
                | JObject (m) ->
                    m
                    |> Seq.sumBy (fun pair -> calculateSum (pair.Value))
                | _ -> 0

        match parseJsonString s with
        | Success (v, _, _) -> calculateSum v
        | Failure (msg, err, _) -> raise (System.Exception(msg))
