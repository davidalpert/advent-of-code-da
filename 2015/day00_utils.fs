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

    // http://www.fssnip.net/2z/title/All-combinations-of-list-elements
    // This takes something like [1;2;3;4] and returns
    // [4][4; 3][4; 3; 2][4; 3; 2; 1][4; 3; 1][4; 2][4; 2; 1]
    // [4; 1][3][3; 2][3; 2; 1][3; 1][2][2; 1][1]
    let allCombinations lst =
        let rec comb accLst elemLst =
            match elemLst with
            | h :: t ->
                let next =
                    [ h ] :: List.map (fun el -> h :: el) accLst
                    @ accLst

                comb next t
            | _ -> accLst

        comb [] lst

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

    // the cartesian product of a list of lists:
    // https://stackoverflow.com/a/3334871/8997
    let rec cart1 LL =
        match LL with
        | [] -> Seq.singleton []
        | L :: Ls ->
            seq {
                for x in L do
                    for xs in cart1 Ls -> x :: xs
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
