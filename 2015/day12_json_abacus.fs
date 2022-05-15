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
