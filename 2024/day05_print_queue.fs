namespace AdventOfCode

module day05_Print_Queue =

    open System
    open System.Collections.Generic
    open AdventOfCode.utils
    open FSharp.Data.UnitSystems.SI.UnitNames
    open FParsec
    open FParsec.Pipes

    type OrderingRule = int array -> bool
    
    type PageUpdate = int array
    
    let constraintFn x y (pages: int array) =
        let indexOfX = pages |> Array.tryFindIndex (fun p -> p = x)
        let indexOfY = pages |> Array.tryFindIndex (fun p -> p = y)
        match (indexOfX, indexOfY) with
        | (Some i, Some j) -> i < j
        | _ -> true
        
    module parser =
        let ws = spaces
        let ch = pchar

        let pOrderingRule =
            %% ws -- +.pint32 -? ch '|' -- +.pint32 -|> constraintFn

        let pPageUpdate =
            %% ws -- +.(pint32 * (qty.[1..] / ','))
            -|> fun a -> a |> Array.ofSeq

        let pInput =
            %% +.(pOrderingRule * (qty.[1..]))
            -- ws -- +.(pPageUpdate * (qty.[1..]))
            -|> fun a b -> (a |> Array.ofSeq, b |> Array.ofSeq)

        let parseInput (input:string) =
            mustParse pInput input

        let mustParse p (input:string) =
            match run p input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg

    // adapted from https://stackoverflow.com/a/279587
    // let sort (compareFn: ('a -> 'a  -> 'a array-> bool) option) (arr:'a[]) = 
    //   let arr = arr |> Array.copy
    //   let swap i j = let tmp = arr.[i] in arr.[i] <- arr.[j]; arr.[j] <- tmp
    //   let doCompare = match compareFn with Some(fn) -> fn | None -> fun i j a -> (a.[i] > a.[j])
    //   for i = arr.Length - 1 downto 0 do
    //     for j = 1 to i do
    //       if (doCompare (j - 1) j arr) then swap (j-1) j
    //   arr
    //   
    // let orderPages (constraints: OrderingRule array) (pages: int array) =
    //     let matchesAllConstraints (constraints: OrderingRule array) a b arr =
    //       constraints |> Array.forall (fun c -> c arr)
    //     
    //     pages |> sort (Some(matchesAllConstraints constraints))
    //
    let orderPages (constraints: OrderingRule array) (pages: int array) =
        let swap i j (arr:'int array) =
           let tmp = arr.[i]
           arr.[i] <- arr.[j]
           arr.[j] <- tmp

        seq { for i in 0 .. (pages.Length - 2) do i }
        |> Seq.fold (fun arr i -> 
            let j = i + 1
            if constraints |> Array.forall (fun c -> c pages) |> not then
                swap i j arr
            arr) pages
    
    let solve (constraints: OrderingRule array, updates: PageUpdate array) =
        let validUpdates =
            updates |> Array.filter (fun u -> constraints |> Array.forall (fun c -> c u))
            
        let findMiddle (pages: int array) =
            let middle = pages.Length / 2
            pages.[middle]
            
        let part1 = validUpdates |> Array.map findMiddle |> Array.sum
        let part2 = updates |> Array.except validUpdates |> Array.map (orderPages constraints)
        
        (part1, part2)