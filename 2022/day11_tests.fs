namespace AdventOfCode

module Day11 =

    open AdventOfCode.MonkeyInTheMiddle
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"""

    let puzzleInput =
        """
Monkey 0:
  Starting items: 73, 77
  Operation: new = old * 5
  Test: divisible by 11
    If true: throw to monkey 6
    If false: throw to monkey 5

Monkey 1:
  Starting items: 57, 88, 80
  Operation: new = old + 5
  Test: divisible by 19
    If true: throw to monkey 6
    If false: throw to monkey 0

Monkey 2:
  Starting items: 61, 81, 84, 69, 77, 88
  Operation: new = old * 19
  Test: divisible by 5
    If true: throw to monkey 3
    If false: throw to monkey 1

Monkey 3:
  Starting items: 78, 89, 71, 60, 81, 84, 87, 75
  Operation: new = old + 7
  Test: divisible by 3
    If true: throw to monkey 1
    If false: throw to monkey 0

Monkey 4:
  Starting items: 60, 76, 90, 63, 86, 87, 89
  Operation: new = old + 2
  Test: divisible by 13
    If true: throw to monkey 2
    If false: throw to monkey 7

Monkey 5:
  Starting items: 88
  Operation: new = old + 1
  Test: divisible by 17
    If true: throw to monkey 4
    If false: throw to monkey 7

Monkey 6:
  Starting items: 84, 98, 78, 85
  Operation: new = old * old
  Test: divisible by 7
    If true: throw to monkey 5
    If false: throw to monkey 4

Monkey 7:
  Starting items: 98, 89, 78, 73, 71
  Operation: new = old + 4
  Test: divisible by 2
    If true: throw to monkey 3
    If false: throw to monkey 2
"""

    [<Fact>]
    let ``2022 - Day 11 - part 1 - parse an operation`` () =
        let input = "Operation: new = old * 19"
        
        input
        |> parser.mustParse parser.pOperation
        |> should equal (MultiplyBy 19)

    [<Fact>]
    let ``2022 - Day 11 - part 1 - parse a monkey`` () =
        let input = """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
"""
        let monkey =
            input
            |> parser.mustParse parser.pMonkey
            
        monkey.name |> should equal "Monkey 0"
        monkey.operation |> should equal (MultiplyBy 19)
        monkey.items |> should equal [{ worryLevel = 79 }; { worryLevel = 98 }]
        monkey.throwsTo 23 |> should equal 2 
        monkey.throwsTo 24 |> should equal 3
        
    [<Fact>]
    let ``2022 - Day 11 - part 1 - parse all the monkeys`` () =
        let barrel =
            exampleInput
            |> parser.mustParse parser.pBarrel
            
        barrel.Length |> should equal 4
            
    [<Theory>]
    [<InlineData(1, """
Monkey 0: 20, 23, 27, 26
Monkey 1: 2080, 25, 167, 207, 401, 1046
Monkey 2:
Monkey 3:""")>]
    [<InlineData(2, """
Monkey 0: 695, 10, 71, 135, 350
Monkey 1: 43, 49, 58, 55, 362
Monkey 2:
Monkey 3:""")>]
    [<InlineData(20, """
Monkey 0: 10, 12, 14, 26, 34
Monkey 1: 245, 93, 53, 199, 115
Monkey 2:
Monkey 3:""")>]
    let ``2022 - Day 11 - part 1 - example - the barrel after round n`` (n:int, expected:string) =
        exampleInput
        |> parser.parse
        |> afterRound part1_adjustLevel n 
        |> toString
        |> should equal (expected.Trim())
        
    [<Theory>]
    [<InlineData(20, """
Monkey 0 inspected items 101 times.
Monkey 1 inspected items 95 times.
Monkey 2 inspected items 7 times.
Monkey 3 inspected items 105 times.""")>]
    let ``2022 - Day 11 - part 1 - example - activity chart after round n`` (n:int, expected:string) =
        exampleInput
        |> parser.parse
        |> afterRound part1_adjustLevel n
        |> toActivityChart
        |> should equal (expected.Trim())

    [<Fact>]
    let ``2022 - Day 11 - part 1 - example`` () =
        exampleInput
        |> part1_what_is_the_level_of_monkey_business_after_n_rounds_of_simian_shenanigans 20
        |> should equal 10605L
        
    [<Fact>]
    let ``2022 - Day 11 - part 1`` () =
        puzzleInput
        |> part1_what_is_the_level_of_monkey_business_after_n_rounds_of_simian_shenanigans 20
        |> printfn "2022 - Day 11 - Part 1: %A"

    [<Theory>]
    [<InlineData(1, """
Monkey 0 inspected items 2 times.
Monkey 1 inspected items 4 times.
Monkey 2 inspected items 3 times.
Monkey 3 inspected items 6 times.""")>]
    [<InlineData(20, """
Monkey 0 inspected items 99 times.
Monkey 1 inspected items 97 times.
Monkey 2 inspected items 8 times.
Monkey 3 inspected items 103 times.""")>]
    [<InlineData(1000, """
Monkey 0 inspected items 5204 times.
Monkey 1 inspected items 4792 times.
Monkey 2 inspected items 199 times.
Monkey 3 inspected items 5192 times.""")>]
    [<InlineData(10000, """
Monkey 0 inspected items 52166 times.
Monkey 1 inspected items 47830 times.
Monkey 2 inspected items 1938 times.
Monkey 3 inspected items 52013 times.""")>]
    let ``2022 - Day 11 - part 2 - example - activity chart after round n`` (n:int, expected:string) =
        exampleInput
        |> parser.parse
        |> afterRound part2_adjustLevel n
        |> toActivityChart
        |> should equal (expected.Trim())
        
    [<Fact>]
    let ``2022 - Day 11 - part 2 - example`` () =
        exampleInput
        |> part2_what_is_the_level_of_monkey_business_after_n_rounds_of_simian_shenanigans 10000
        |> should equal 2713310158L

    [<Fact>]
    let ``2022 - Day 11 - part 2`` () =
        puzzleInput
        |> part2_what_is_the_level_of_monkey_business_after_n_rounds_of_simian_shenanigans 10000
        |> printfn "2022 - Day 11 - Part 2: %A"
