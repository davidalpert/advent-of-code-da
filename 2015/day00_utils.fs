namespace AdventOfCode

module utils =

  open System.Text.RegularExpressions

  // http://www.fssnip.net/29/title/Regular-expression-active-pattern
  let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

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
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

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
