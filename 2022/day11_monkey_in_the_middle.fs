namespace AdventOfCode

module MonkeyInTheMiddle =

    open AdventOfCode.Input
    open FParsec
    open FParsec.Pipes

    type Item = { worryLevel: int }
        with
            static member fromInt i = { worryLevel = i }
    
    type Operation =
    | Add of int
    | MultiplyBy of int
    | Square
    
    type Test = int -> int // when divisible by int where do we throw the item?
    
    type Monkey = {
        name: string
        items: Item list
        operation: Operation
        throwsTo: Test
        inspectionCount: int
    }
    
    module parser =
        let ws = spaces
        let pint : Parser<int,unit> = %% +.pint32 -%> auto
        
        let pName =
            %% %"Monkey" -- ws -- +.pint -- %':' -- ws -%> auto
            
        let pStartingItems =
            %% ws -- %"Starting items:" -- ws -- +.(pint32 * (qty[0..] / ", "))
            -|> fun items -> items |> Seq.map Item.fromInt |> List.ofSeq
            
        let pAdd = %% ws -- %"new = old + " -? +.pint -%> Add
        let pSquare = %% ws -- %"new = old * " -? %"old" -|> Square
        let pMultiply = %% ws -- %"new = old * " -? +.pint -%> MultiplyBy
        let pOperation =
            %% ws -- %"Operation:" -- ws -- +.(%[pSquare;pMultiply;pAdd]) -%> auto
            
        let pTest : Parser<Test,unit> =
            %% ws -- %"Test: divisible by" -- ws -- +.pint
            -- ws -- %"If true: throw to monkey " -- +.pint
            -- ws -- %"If false: throw to monkey " -- +.pint
            -|> fun divisibleBy whenTrue whenFalse ->
                fun n -> if n % divisibleBy = 0 then whenTrue else whenFalse
                
        let pMonkey =
            %% ws -? +.pName
            -- +.pStartingItems
            -- +.pOperation
            -- +.pTest
            -|> fun number startingItems operation test -> {
                name = (sprintf $"Monkey %d{number}");
                items = startingItems;
                operation = operation;
                throwsTo = test;
                inspectionCount = 0;
            }
            
        let pBarrel =
            %% +.(pMonkey * qty[1..]) -|> fun b -> b |> Array.ofSeq
            
        let mustParse p (input:string) =
            match run p (input.Trim()) with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg
            
        let parse (input:string) = mustParse pBarrel input
        
    let joinBy (sep:string) (strings:string seq) =
        System.String.Join(sep, strings)
        
    // formatting a barrel like the example helps me write tests that look like the examples
    let toString (barrel:Monkey[]) =
        barrel
        |> Array.map (fun m ->
                let items = m.items |> List.map (fun i -> i.worryLevel.ToString()) |> joinBy ", "
                (sprintf $"%s{m.name}: %s{items}").Trim()
            )
        |> joinBy "\n"
        
    let toActivityChart (barrel:Monkey[]) =
        barrel
        |> Array.map (fun m ->
                (sprintf $"%s{m.name} inspected items %d{m.inspectionCount} times.").Trim()
            )
        |> joinBy "\n"
        
    let addToEnd item ll =
        (item :: (ll |> List.rev)) |> List.rev
        
    let afterOneRound (barrel:Monkey[]) =
        let throwTo m item =
            barrel[m] <- {
                barrel[m] with
                    items = barrel[m].items |> addToEnd item
            }
            
        seq { 0 .. (barrel.Length - 1) }
        |> Seq.iter (fun m ->
                let monkey = barrel[m]
                monkey.items
                |> Seq.iter (fun item ->
                        // printfn $"Monkey %d{m}"
                        // printfn $"  Monkey inspects an item with a worry level of %d{item.worryLevel}"
                        
                        let inspectionLevel =
                            match monkey.operation with
                            | Add(n) -> item.worryLevel + n
                            | MultiplyBy(n) -> item.worryLevel * n
                            | Square -> item.worryLevel * item.worryLevel
                        // printfn $"    worry level is '%A{monkey.operation}' to %d{inspectionLevel}"
                            
                        let relievedLevel = inspectionLevel / 3
                        // printfn $"    monkey gets bored with item; worry level is / 3 to %d{relievedLevel}"
                        
                        let toMonkey = monkey.throwsTo relievedLevel
                        // printfn $"    item with worry level %d{relievedLevel} is thrown to monkey %d{toMonkey}"
                        
                        relievedLevel |> Item.fromInt |> throwTo toMonkey
                    )
                
                // now all this monkey's items have been thrown
                barrel[m] <- {
                    monkey with
                        items = []
                        inspectionCount = monkey.inspectionCount + monkey.items.Length
                }
                // printfn $"%s{(barrel |> toString)}"
            )
            
        barrel
        
    let rec afterRound n (barrel: Monkey[]) =
        match n with
        | 0 -> barrel
        | _ -> afterRound (n-1) (afterOneRound barrel)
        
    let shenanigans (barrel: Monkey[]) =
        barrel
        |> Array.map (fun m -> m.inspectionCount)
        |> Array.sortDescending
        |> Array.take 2
        |> Array.fold (*) 1
        
    let part1_what_is_the_level_of_monkey_business_after_n_rounds_of_simian_shenanigans n (input:string) =
        input
        |> parser.parse
        |> afterRound n
        |> shenanigans
