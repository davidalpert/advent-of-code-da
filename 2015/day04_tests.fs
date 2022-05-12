namespace AdventOfCode.Tests

module Day04 =

  // If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
  // If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....

  open AdventOfCode.MiningAdventCoins
  open Xunit
  open FsUnit.Xunit

  [<Theory>]
  [<InlineData("abcdef", 5, 609043)>]
  [<InlineData("pqrstuv", 5, 1048970)>]
  [<InlineData("bgvyzdsv", 5, 254575)>] // <- day 4 - part 1 - input
  [<InlineData("bgvyzdsv", 6, 1038736)>] // <- day 4 - part 2 - input
  let ``Day 4`` (input:string, lengthOfPrefix:int, expectedN:int) =
    input
    |> firstCoinSuffix lengthOfPrefix
    |> should equal expectedN
