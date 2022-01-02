namespace AdventOfCode

module utils =
  // https://rolfsuter.ch/code/f-transpose-a-list-of-lists-transpose-2d-matrix/
  let rec transpose =
    function
    | (_ :: _) :: _ as M ->
        List.map List.head M
        :: transpose (List.map List.tail M)
    | _ -> []

  // debug helpers
  let ( ~~~ ) =
    // printfn
    sprintf

  let ( ~~~~ ) =
    // printfn
    sprintf

  let ( ~~~~~ ) =
    // printfn
    sprintf
