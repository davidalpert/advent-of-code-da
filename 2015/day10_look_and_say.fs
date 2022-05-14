namespace AdventOfCode

open AdventOfCode.Input
open AdventOfCode.utils

module LookAndSay =

  open System
  open FParsec
  open FParsec.Pipes

  let ch = pchar
  let ws = spaces

  type DigitReadAloud = {
    nTimes : int
    digit : char
  }

  let pDigitSet : Parser<DigitReadAloud,unit> =
    fun stream ->
      let r = digit stream
      match r.Status with
      | Ok ->
        // printfn "found: %c" r.Result
        let rest = manySatisfy (fun c -> c = r.Result) stream
        match rest.Status with
        | Ok ->
          // printfn "- done with: %s (%d)" rest.Result rest.Result.Length
          Reply({
            digit = r.Result;
            nTimes = (rest.Result.Length + 1);
          })
        | _ ->
          // printfn "- done"
          Reply({
            digit = r.Result;
            nTimes = 1;
          })
      | _ ->
        Reply(Error, r.Error)

  let p =
    %% +.(pDigitSet * qty.[1..])
    -|> fun digits ->
      digits
      // |> Seq.map ( fun s ->
      //   printfn "< %A" s
      //   s
      // )
      |> Seq.map (fun d -> sprintf "%d%c" (d.nTimes) (d.digit))
      // |> Seq.map ( fun s ->
      //   printfn "> %s" s
      //   s
      // )
      |> String.Concat

  let lookAndSay (s:string) =
    match s.Length with
    | 0 -> s
    | 1 -> "1" + s
    | _ ->
      match run p s with
      | Success(r,_,_) -> r
      | Failure(msg,_,_) -> raise (Exception(msg))

  let lookAndSayNTimes (n:int) (debug:bool) (s:string) =
    seq { 1 .. n }
    |> Seq.fold (fun ss t ->
      if debug then
        printfn ">%20d< %s" t ss
      lookAndSay ss
    ) s