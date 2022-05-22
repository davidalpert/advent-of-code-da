namespace AdventOfCode

module utils =

    open System.Text.RegularExpressions

    // http://www.fssnip.net/29/title/Regular-expression-active-pattern
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    // https://rolfsuter.ch/code/f-transpose-a-list-of-lists-transpose-2d-matrix/
    let rec transpose =
        function
        | (_ :: _) :: _ as M ->
            List.map List.head M
            :: transpose (List.map List.tail M)
        | _ -> []

    // https://stackoverflow.com/a/1231711/8997
    let rec comb n l =
        match n, l with
        | 0, _ -> [ [] ]
        | _, [] -> []
        | k, (x :: xs) -> List.map ((@) [ x ]) (comb (k - 1) xs) @ comb k xs

    // http://www.fssnip.net/fF/title/Combinations-n-choose-k
    let n_choose_k k n =
        let rec choose lo =
            function
            | 0 -> [ [] ]
            | i ->
                [ for j = lo to (Array.length n) - 1 do
                      for ks in choose (j + 1) (i - 1) do
                          yield n.[j] :: ks ] in choose 0 k

    // https://rosettacode.org/wiki/Permutations#F.23
    let rec permuteAll sourceList =
        let rec insert left x right =
            seq {
                match right with
                | [] -> yield left @ [ x ]
                | head :: tail ->
                    yield left @ [ x ] @ right
                    yield! insert (left @ [ head ]) x tail
            }

        seq {
            match sourceList with
            | [] -> yield []
            | head :: tail -> yield! Seq.collect (insert [] head) (permuteAll tail)
        }

// // debug helpers
// let ( ~~~ ) =
//   // printfn
//   sprintf

// let ( ~~~~ ) =
//   // printfn
//   sprintf

// let ( ~~~~~ ) =
//   // printfn
//   sprintf
