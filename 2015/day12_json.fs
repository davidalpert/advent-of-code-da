namespace AdventOfCode

module Json =
    // https://github.com/stephan-tolksdorf/fparsec/blob/1.1.1/Samples/JSON/ast.fs
    module Ast =
        [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
        type Json =
            | JString of string
            | JNumber of float
            | JBool of bool
            | JNull
            | JList of Json list
            | JObject of Map<string, Json>
            member private t.StructuredFormatDisplay =
                match t with
                | JString s -> box ("\"" + s + "\"")
                | JNumber f -> box f
                | JBool b -> box b
                | JNull -> box "null"
                | JList l -> box l
                | JObject m -> Map.toList m :> obj

    // https://github.com/stephan-tolksdorf/fparsec/blob/1.1.1/Samples/JSON/parser.fs
    module Parser =

        open FParsec
        open Ast

        // This is a general JSON parser that will parse any JSON file into an AST.
        // See e.g. http://www.json.org/, for a specification of JSON.

        // The FParsec tutorial discusses this parser in detail.

        // Note that in typical applications you often don't need to parse any general
        // JSON file, but only files describing objects of a certain type. In those cases
        // it might be more convenient to parse the input with specialized parsers
        // instead of using the indirect approach via an intermediate AST. The parser
        // definitions below should be useful in any case.

        let jnull = stringReturn "null" JNull
        let jtrue = stringReturn "true" (JBool true)
        let jfalse = stringReturn "false" (JBool false)

        let jnumber = pfloat |>> JNumber // pfloat will accept a little more than specified by JSON
        // as valid numbers (such as NaN or Infinity), but that makes
        // it only more robust

        let str s = pstring s

        let stringLiteral =
            let escape =
                anyOf "\"\\/bfnrt"
                |>> function
                    | 'b' -> "\b"
                    | 'f' -> "\u000C"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | c -> string c // every other char is mapped to itself

            let unicodeEscape =
                /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
                let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

                str "u"
                >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    (hex2int h3) * 4096
                    + (hex2int h2) * 256
                    + (hex2int h1) * 16
                    + hex2int h0
                    |> char
                    |> string)

            let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
            let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

            between (str "\"") (str "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)

        let jstring = stringLiteral |>> JString

        // jvalue, jlist and jobject are three mutually recursive grammar productions.
        // In order to break the cyclic dependency, we make jvalue a parser that
        // forwards all calls to a parser in a reference cell.
        let jvalue, jvalueRef = createParserForwardedToRef () // initially jvalueRef holds a reference to a dummy parser

        let ws = spaces // skips any whitespace

        let listBetweenStrings sOpen sClose pElement f =
            between
                (str sOpen)
                (str sClose)
                (ws >>. sepBy (pElement .>> ws) (str "," .>> ws)
                 |>> f)

        let keyValue = tuple2 stringLiteral (ws >>. str ":" >>. ws >>. jvalue)

        let jlist = listBetweenStrings "[" "]" jvalue JList
        let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

        do
            jvalueRef
            := choice [ jobject
                        jlist
                        jstring
                        jnumber
                        jtrue
                        jfalse
                        jnull ]

        let json = ws >>. jvalue .>> ws .>> eof

        let parseJsonString str = run json str

        // UTF8 is the default, but it will detect UTF16 or UTF32 byte-order marks automatically
        let parseJsonFile fileName encoding =
            runParserOnFile json () fileName encoding

        let parseJsonStream stream encoding =
            runParserOnStream json () "" stream System.Text.Encoding.UTF8
