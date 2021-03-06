namespace AdventOfCode

module Day10 =

  open Input
  open Xunit
  open FsUnit.Xunit

  // [<Theory>]
  [<InlineData("()", "v:()")>]
  [<InlineData("([])", "v:(v:[])")>]
  [<InlineData("{()()()}", "v:{v:()v:()v:()}")>]
  [<InlineData("<([{}])>", "v:<v:(v:[v:{}])>")>]
  [<InlineData("[<>({}){}[([])<>]]", "v:[v:<>v:(v:{})v:{}v:[v:(v:[])v:<>]]")>]
  [<InlineData("(((((((((())))))))))", "v:(v:(v:(v:(v:(v:(v:(v:(v:(v:())))))))))")>]
  [<InlineData("(]", "c:(] expected ')' but got ']'")>]
  [<InlineData("{()()()>", "c:{v:()v:()v:()> expected '}' but got '>'")>]
  [<InlineData("(((()))}", "c:(v:(v:(v:()))} expected ')' but got '}'")>]
  [<InlineData("<([]){()}[{}])", "c:<v:(v:[])v:{v:()}v:[v:{}] expected '>' but got ')'")>]
  let ``Day 10 - tests - parse chunks`` (input:string, expected:string) =
    let r = NavigationParser.parseChunk input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> result.toAnnotatedString
                    |> should equal expected

  [<Theory>]
  [<InlineData("()", "Valid      | v:()")>]
  [<InlineData("[}", "Corrupted  | c:[} expected ']' but got '}'")>]
  [<InlineData("[<>[]}", "Corrupted  | c:[v:<>v:[]} expected ']' but got '}'")>]
  [<InlineData("[<>[]}{[]{[(<()>", "Corrupted  | c:[v:<>v:[]} expected ']' but got '}' ignoring: '{[]{[(<()>'")>]
  [<InlineData("{([(<{}[<>[]}>{[]{[(<()>", "Corrupted  | i:{i:(i:[i:(i:<v:{}c:[v:<>v:[]} expected ']' but got '}' ignoring: '>{[]{[(<()>'")>]
  [<InlineData("[({(<(())[]>[[{[]{<()<>>", "Incomplete | i:[i:(i:{i:(v:<v:(v:())v:[]>i:[i:[i:{v:[]i:{v:<v:()v:<>>")>]
  let ``Day 10 - tests - parse lines`` (input:string, expected:string) =
    let r = NavigationParser.parseLine input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> result.toAnnotatedString
                    |> should equal expected

  [<Theory>]
  [<InlineData("[({(<(())[]>[[{[]{<()<>>", "}}]])})]")>]
  [<InlineData("[(()[<>])]({[<{<<[]>>(",   ")}>]})")>]
  [<InlineData("(((({<>}<{<{<>}{[]{[]{}",  "}}>}>))))")>]
  [<InlineData("{<[[]]>}<{[{[{[]{()[[[]",  "]]}}]}]}>")>]
  [<InlineData("<{([{{}}[<[[[<>{}]]]>[]]", "])}>")>]
  let ``Day 10 - tests - calculate completion string`` (input:string, expected:string) =
    let r = NavigationParser.parseLine input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> result.completionString
                    |> should equal expected

  [<Theory>]
  [<InlineData("[({(<(())[]>[[{[]{<()<>>", 288957)>]
  [<InlineData("[(()[<>])]({[<{<<[]>>(",   5566)>]
  [<InlineData("(((({<>}<{<{<>}{[]{[]{}",  1480781)>]
  [<InlineData("{<[[]]>}<{[{[{[]{()[[[]",  995444)>]
  [<InlineData("<{([{{}}[<[[[<>{}]]]>[]]", 294)>]
  let ``Day 10 - tests - calculate completion score`` (input:string, expected:int) =
    let r = NavigationParser.parseLine input

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> result.completionScore
                    |> should equal (expected |> int64)

  [<Fact>]
  let ``Day 10 - tests - parse subsystem`` () =
    let r = NavigationParser.parseSubsystem day10sample

    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> result.toAnnotatedString
                    |> should equal ("""
Incomplete | i:[i:(i:{i:(v:<v:(v:())v:[]>i:[i:[i:{v:[]i:{v:<v:()v:<>>
Incomplete | v:[v:(v:()v:[v:<>])] | i:(i:{i:[i:<i:{v:<v:<v:[]>>i:(
Corrupted  | i:{i:(i:[i:(i:<v:{}c:[v:<>v:[]} expected ']' but got '}' ignoring: '>{[]{[(<()>'
Incomplete | i:(i:(i:(i:(v:{v:<>}i:<i:{i:<v:{v:<>}i:{v:[]i:{v:[]v:{}
Corrupted  | i:[i:[i:<c:[v:(v:[])) expected ']' but got ')' ignoring: '<([[{}[[()]]]'
Corrupted  | i:[i:{i:[i:{c:(v:{}] expected ')' but got ']' ignoring: '{}}([{[{{{}}([]'
Incomplete | v:{v:<v:[v:[]]>} | i:<i:{i:[i:{i:[i:{v:[]i:{v:()i:[i:[v:[]
Corrupted  | i:[i:<i:(i:<i:(i:<i:(c:<v:{}) expected '>' but got ')' ignoring: ')><([]([]()'
Corrupted  | i:<i:{i:(i:[i:(c:[v:[v:(v:<>v:())v:{}]> expected ']' but got '>' ignoring: '(<<{{'
Incomplete | i:<i:{i:(i:[v:{v:{}}v:[v:<v:[v:[v:[v:<>v:{}]]]>v:[]]
""".Trim())

  [<Fact>]
  let ``Day 10 - sample`` () =
    let r = NavigationParser.parseSubsystem day10sample

    // part 1
    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> result.totalSyntaxScore
                    |> should equal ( 26397 |> int64 )

    // part 2
    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> result.winningCompletionScore
                    |> should equal ( 288957 |> int64 )

  [<Fact>]
  let ``Day 10 - calculation`` () =
    let r = NavigationParser.parseSubsystem day10data

    // part 1
    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> result.totalSyntaxScore
                    |> should equal ( 167379 |> int64 )

    // part 2
    match r with
    | Error(e)   -> failwith e
    | Ok(result) -> result.winningCompletionScore
                    |> should equal ( 2776842859L |> int64 )