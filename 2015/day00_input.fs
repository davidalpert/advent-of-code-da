namespace AdventOfCode

module Input =
  let splitToTrimmedLines (input:string) =
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())

  let splitToTrimmedLinesVerbose (input:string) =
    printfn "input: %A" input
    printfn "input.trimmed: %A" (input.Trim())
    printfn "input.trimmed.split: %A" (input.Trim().Split("\n"))
    input.Trim().Split("\n")
    |> Seq.map (fun s -> s.Trim())

  let integersFromInput (input: string) =
    splitToTrimmedLines input
    |> Seq.map (fun s -> s |> int)

  //#region day01

  let day01data = "()()(()()()(()()((()((()))((()((((()()((((()))()((((())(((((((()(((((((((()(((())(()()(()((()()(()(())(()((((()((()()()((((())((((((()(()(((()())(()((((()))())(())(()(()()))))))))((((((((((((()())()())())(())))(((()()()((((()(((()(()(()()(()(()()(()(((((((())(())(())())))((()())()((((()()((()))(((()()()())))(())))((((())(((()())(())(()))(()((((()())))())((()(())(((()((((()((()(())())))((()))()()(()(()))))((((((((()())((((()()((((()(()())(((((()(()())()))())(((()))()(()(()(()((((()(())(()))(((((()()(()()()(()(((())())(((()()(()()))(((()()(((())())(()(())())()()(())()()()((()(((()(())((()()((())()))((()()))((()()())((((()(()()(()(((()))()(()))))((()(((()()()))(()(((())()(()((()())(()(()()(()())(())()(((()(()())()((((()((()))))())()))((()()()()(())()())()()()((((()))))(()(((()()(((((((())()))()((((()((())()(()())(())()))(()(()())(((((((())))(((()))())))))()))())((())(()()((())()())()))))()((()()())(())((())((((()())())()()()(((()))())))()()))())(()()()(()((((((()()))())()))()(((()(((())((((()()()(()))())()()))))())()))())((())()())(((((())())((())())))(((())(((())(((((()(((((())(()(()())())(()(())(()))(()((((()))())()))))())))((()(()))))())))(((((())()))())()))))()))))(((()))()))))((()))((()((()(()(())()())))(()()()(())()))()((((())))))))(())(()((()()))(()))(()))(()((()))))))()()((((()()))()())()))))))()()()))(()((())(()))((()()()())()(((()((((())())))()((((()(()))))))())))()()())()))(()))))(()())()))))))((())))))))())()))()((())())))(()((()))()))(())))))(()))()())()()))((()(()))()()()()))))())()()))())(())()()))()))((()))))()()(()())))))()()()))((((()))()))))(()(())))(()())))((())())(()))()))))()())))()())()())))))))))()()))))())))((())((()))))())))(((()())))))))(()))()()))(()))()))))()())))))())((((()())))))))())))()()))))))))()))()))))()))))))(())))))))))())))))))))))))))())())((())))))))))()))((())))()))))))))())()(()))))))())))))()()()())()(()()()(()())(()))()()()(()())))())())))()))))())))))))()()()()())(())())()())()))))(()()()()()))))()))())())))((()())()())))()))()))))(()())))()))))))))(((()))()()))))))))))))))))))))(()))(()((()))())))())(()))(()(()(())))))()(()))()))()()))))))))))))()((()())(())())()(())))))())()())((()()))))(()()))))())()(())()))))))))))))))))))))()))(()(()())))))))()()((()))()))))))((())))()))))))))((()))())()()))())()()))((()))())))))))))))(()())()))(())((()(()()))(()())(())))()())(()(())()()))))()))()(()))))))(()))))))))))(()))())))))))))())))))())))(())))))()))))(())())))))))))()(()))))()())))())(()))()())))))))))))))())()()))))()))))))())))))()))))(())(()()()()((())()))())(()))((())()))())())(())(()()))))()))(())()()((())(())))(())))()))())))))))))()(((((())())))(())()))))(())))((()))()(((((((()))))()()))(())))))()(()))))(()()))()))())))))))(()())()))))))))())))(()))())()))(())()((())())()())())(()(()))))()))))))((()())(())()()(()())))()()))(())(())(()))())))()))(()))()()))((((()))))()))((()()()))))()))()))())))(()))()))))(())))()))())()(()))()())))())))))))())))())))()()))))))(()))())())))()))()()())())))))))))))))())))()))(()()))))())))())()(())))())))))))))))))))))()()())())))))()()()((()(()))()()(())()())()))()))))()()()))))))((()))))))))()(()(()((((((()()((()())))))))))))()))())))))((())())(()))())))())))))())()()())(())))())))()())())(())))))))()()(())))()))())))())())())()))))))))()))(()()()())())())))(())())))))))()()())()))))())))())()(())())))))))()())()))(()()(())())))()(()((()()((()()(((((())(()())()))(())()))(())))(())))))))()))()))((()))()))()))))))))()))))))))((()()())(()))(((()))(())))()))((())(((())))()())))())))))((())))))(())())((((((())())()(()))()(()((()())))((())()(()(()))))(())(()()())(())))())((()(((())())))(((()())())))())()(())())((((()()))))())((()))()()()()(())(((((((()()()((()))())(()())))(())())((((()()(()))))()((())))((())()))()(((()))())))()))((()(()))(())(()((((())((((()()(()()))(((())(()))))((((()(()))(())))))((()))(()))((()(((()(()))(()(()((()(())(()(()(()(()()((()))())(((())(()(()))))(()))()()))(())))(())()(((())(()))()((((()()))))())(()))))((())()((((()(((()))())())(((()))()())((())(())())(())()(())()(()()((((((()()))))()()(((()()))))()())()(((()(()))(()(()())(()(()))))(((((()(((())())))))(((((()((()()((())())((((((()(())(()()((()()()()()()()(()()))()(((()))()))(((((((())(((()((()())()((((())(((()(())))()((()(()()()((())((()())()))()))())))())((((((()))(()(()()()))(()((()(()(()))()((()(((()()()((())(((((())()(()))())())((()(())))(()(()())(())((())())())(((()()()(())))))())(()))))))()))))))())((()()()))((()((((((()))(((()((((()()()(((()))())()(()()(((()((()()()()())()()))()()()(()(())((()))))(()))())))))))()(()()(((((())()(()(((((()((()(()()())(()((((((((()((((((())()((((()()()((()((()((((((()))((())))))))())()))((()(()))()(()()(()((())((()()((((((((((((()())(()()()))((((()((((((())(()))())(()()((()()))()(((((((()((()()((((((()(((())))((())))((((((((()()(((((((())(((((()())(((())((())()((((()(((((((()(()(((()((((((()(((()(((((((((((()()((()()(()))((()()(((()(((())))((((())()(()(((())()(()(((())(((((((((((()))())))((((((())((()()((((()())())((((()()))((())(((((()(()()(()()()((())(()((()()((((()(((((()((()(()((((()())((((((()(((((()()(()(()((((())))(())(())(())((((()(()()((((()((((()()((()((((((())))(((((()))))()))(()((((((((()(((())())(((())))(()(()((())(((()((()()(((((()((()()(((())()(()))(((((((())(()(((((()))((()((()((()))(())())((((()((((())()(()))(((()(((((((((((((((())(((((((((()))(((()(()()()()((((((()((())()((((((((()(())(((((((((((()(()((())()((()()(()(()()((((()()((())(()((()()(()()((((()(((((((())))((((())(())()(((()()((()()((((()((()(((()((())(((()()()((((()((((()()(()(()((((((((())(()(((((())(()())(((((((()())()(()((((()((())(()()())((((()()(((()((((())(())(()()(((((((((()()))()(((())(()(()((((((())(()()())(()))()()(((()(((()((())(()(((((((()(()(()((()(((((()(()((()(()((((((()((((()()((((()(((()((())(()(()((()()((((()()(())()(())(((())(()((((((((()())(((((((((()(())()((((())))()))()()(((((()()((((((())(()()(((()(()(((((((()(()(((((((())(())((((()((()(())))((((()()())(()))((()())((((()(((((()(()(())(()(()()())(((((()(((((()((((()()((((((((()()))(()((((((())((((())()(()(((()()()(((()(()(())(())(((((()(())())((((())(())(()(((()(((((())((((())())((()(((((((()(((())(()(()))(((((((((()((()((()()(()((((())(((()((())((((())(()(((()(((()(()((((()(((())(()(((()(()()(()(()((()()(()())(())())((()(()(((()(((()(((()()(((((((((()(((((((((()()(((()(((()())((((()(()(((()()()((())((((((((((())(()(((()((((()())((((()((()))(((()()()(((((()(((((((())((()())(()((((())((((((((())(()((()((((((((((()()((()((()()))(((()())()())()(((()())()()(()(()(((((((())()))(())()))())()()((())()((()((((()((()((())(((((()((((((()(())))(()))())(((()))((()()(()(((()))((((())()(((()))))()(()(())()(((((())(()(()(())(())()((()()()((((()(())((()())(()(()))(()(()(()()(())()()(()((())()((()))))()))((()(()()()()((()())(()))())()(()(((((((((())())((()((()((((((())()((((())(((())((()(()()()((())(()((())(((()((((()()((()(()(((((())()))()((((((()))((())(((()()))(((())(())()))(((((((())(())())()(())(((((()))()((()))()(()()((()()()()()())((((((("

  //#endregion

  let day02data = """
29x13x26
11x11x14
27x2x5
6x10x13
15x19x10
26x29x15
8x23x6
17x8x26
20x28x3
23x12x24
11x17x3
19x23x28
25x2x25
1x15x3
25x14x4
23x10x23
29x19x7
17x10x13
26x30x4
16x7x16
7x5x27
8x23x6
2x20x2
18x4x24
30x2x26
6x14x23
10x23x9
29x29x22
1x21x14
22x10x13
10x12x10
20x13x11
12x2x14
2x16x29
27x18x26
6x12x20
18x17x8
14x25x1
30x15x22
17x18x7
28x23x24
15x12x25
14x7x20
29x23x8
24x5x22
6x22x8
1x15x26
14x5x1
24x28x28
17x23x23
4x15x7
23x8x11
6x15x1
23x18x13
17x1x26
23x13x17
2x18x8
22x22x1
10x22x6
28x29x20
22x21x25
14x8x23
12x30x14
8x7x5
3x30x15
4x3x29
25x18x3
16x7x16
4x3x8
9x16x30
20x28x3
28x24x6
4x18x2
23x18x5
22x4x30
15x30x9
7x12x12
3x22x29
12x1x9
9x2x25
17x11x10
25x24x7
7x27x26
26x4x12
29x2x26
19x24x12
23x23x3
26x28x16
18x4x16
25x30x18
29x19x19
16x3x27
29x25x29
18x19x5
14x21x30
19x13x26
19x10x15
9x4x7
18x6x6
24x25x29
9x12x27
15x3x22
30x17x21
18x19x28
9x11x12
8x28x22
11x3x4
28x17x20
24x18x15
11x12x13
6x19x24
28x4x5
28x22x23
13x29x2
9x16x15
29x28x1
10x18x30
19x11x12
26x28x25
23x17x13
25x1x21
17x1x27
17x27x28
28x13x15
14x13x25
11x29x7
22x29x5
13x6x14
23x18x13
25x7x17
18x9x20
21x11x2
28x11x13
13x25x1
19x29x25
16x29x4
10x21x10
7x25x17
5x9x3
1x15x6
8x27x29
23x6x30
22x22x29
6x20x30
26x25x29
10x19x19
20x30x9
5x30x24
17x10x27
30x14x30
8x17x4
7x18x6
3x5x4
24x17x15
14x20x17
22x27x15
18x14x15
23x9x11
21x16x29
7x18x21
9x3x29
10x13x4
2x30x4
23x20x4
8x22x21
29x28x4
13x16x25
21x9x11
7x26x26
13x23x30
19x7x10
9x23x21
21x9x17
9x21x15
20x29x22
23x13x15
19x25x2
12x11x30
20x21x6
21x6x17
24x26x9
29x21x29
29x26x16
6x16x1
2x12x6
6x7x20
7x2x22
6x22x4
13x11x27
25x27x14
11x8x6
26x11x14
30x3x29
27x21x20
15x16x26
6x22x10
11x9x25
23x13x6
13x9x3
30x22x13
29x23x14
25x19x6
7x29x11
19x18x5
29x25x13
25x24x27
1x9x12
22x9x17
14x12x28
19x21x17
13x25x17
14x25x12
4x14x30
7x15x28
3x6x25
6x2x16
15x19x11
17x30x20
20x23x7
26x21x6
26x29x24
2x4x30
4x22x18
13x3x28
27x6x21
5x3x27
12x7x11
28x11x9
12x9x2
1x22x20
15x13x28
14x19x16
28x20x3
20x4x9
26x7x26
18x19x25
7x1x13
20x23x29
27x26x8
11x15x15
10x21x23
29x2x11
21x28x20
3x18x23
26x17x17
14x26x17
20x7x17
18x12x8
4x8x8
8x15x23
24x29x5
1x25x8
1x28x17
16x18x13
29x24x22
13x16x10
14x7x16
15x11x29
12x15x19
17x6x28
4x3x9
15x16x8
29x27x11
2x24x20
4x21x3
29x24x27
18x22x22
7x8x18
20x7x8
19x9x2
20x17x2
2x29x10
19x25x1
28x9x3
29x27x20
7x21x7
10x4x22
26x8x5
26x14x1
5x27x9
2x18x3
3x27x17
30x17x23
30x11x20
4x6x7
6x29x27
30x16x20
24x30x28
19x20x26
18x1x25
26x12x12
19x15x29
16x21x24
23x13x26
25x16x10
8x9x18
24x14x1
24x15x21
19x9x14
8x23x11
22x2x16
29x9x26
3x16x25
15x20x30
3x11x12
15x2x3
13x7x4
2x7x27
9x26x11
30x24x19
28x17x21
10x8x2
11x15x26
10x12x20
24x24x27
25x26x16
13x4x20
25x13x11
12x22x3
20x7x1
12x18x6
26x8x20
14x2x7
23x12x1
26x24x24
27x26x23
26x17x5
17x24x2
26x5x6
23x5x1
5x18x30
24x21x19
5x28x11
21x20x14
25x4x22
26x24x11
7x5x8
13x1x30
5x1x6
14x5x2
8x11x7
13x20x1
17x30x14
29x22x10
12x26x3
27x17x3
26x27x4
5x26x17
22x11x19
8x26x3
24x19x22
7x1x4
6x27x30
4x28x14
16x14x18
4x5x20
19x25x4
15x15x1
10x14x14
16x18x24
21x27x15
5x5x10
1x7x13
16x2x8
13x15x11
3x25x10
20x29x8
12x3x2
10x13x12
25x27x1
11x30x19
7x19x13
27x6x18
16x21x19
21x29x5
16x23x12
29x19x15
5x5x10
27x15x1
13x16x22
29x19x5
8x12x9
3x18x5
13x25x3
5x9x21
10x20x16
9x9x11
23x21x1
22x2x15
27x8x13
23x7x3
26x30x15
29x15x16
16x27x13
2x18x9
10x27x8
20x9x25
10x2x17
16x13x13
21x26x1
27x26x24
9x30x16
19x17x28
25x15x1
10x26x6
10x11x11
5x26x25
30x4x15
9x8x23
14x25x7
8x28x8
28x18x24
4x4x25
16x25x11
17x27x8
15x16x9
24x13x21
17x3x27
27x5x26
8x27x12
29x2x8
24x23x30
1x30x21
6x18x20
13x14x12
25x30x23
24x6x24
12x7x21
11x6x8
8x30x30
26x3x12
28x6x5
18x7x1
7x6x20
14x16x18
11x22x15
4x20x10
19x24x19
8x24x11
4x9x10
6x6x22
10x9x29
1x5x28
19x25x29
20x30x3
15x13x13
9x9x24
20x14x29
26x24x13
2x25x8
10x26x2
12x19x12
18x6x20
4x5x14
26x27x10
16x26x20
3x21x15
2x26x18
14x11x17
26x26x25
10x1x11
17x19x19
27x28x26
9x2x10
19x30x15
23x30x14
15x3x20
2x14x22
21x18x8
22x4x29
19x6x29
9x26x29
16x10x9
22x12x22
13x28x14
25x14x28
28x3x30
10x17x1
10x27x22
10x23x19
14x25x9
11x24x8
30x25x10
22x13x28
2x7x6
11x20x8
9x22x14
19x16x9
11x24x4
11x17x2
6x4x10
26x10x10
12x14x5
27x10x3
15x3x6
11x7x19
22x10x12
21x26x10
13x20x3
27x8x8
1x24x23
24x9x22
23x17x23
3x28x19
2x20x28
23x17x24
26x1x4
4x1x12
5x6x16
13x22x13
25x21x21
20x21x12
9x24x25
17x16x12
12x28x9
18x16x27
29x12x2
30x12x15
24x11x10
4x9x22
4x24x5
19x11x5
6x25x6
1x20x17
22x8x21
11x26x4
16x19x3
8x12x8
13x2x18
10x5x11
8x12x17
21x2x5
26x17x26
23x18x17
28x11x14
1x4x27
29x5x28
5x9x10
5x7x25
20x15x27
15x11x17
12x14x1
29x14x4
18x14x18
14x25x24
26x14x18
13x8x11
30x1x23
3x4x12
12x24x9
8x6x16
14x15x30
12x30x8
22x11x18
16x30x28
17x18x4
13x14x23
2x28x8
3x28x30
29x30x8
4x6x26
6x30x17
11x30x30
19x4x3
12x15x20
22x28x4
26x30x2
6x12x7
1x10x5
25x29x7
17x9x18
16x21x29
21x14x7
15x16x11
26x6x15
8x24x7
2x20x4
2x9x3
19x8x13
18x7x22
27x14x17
2x13x8
18x15x26
15x27x27
18x11x15
1x29x20
21x12x11
20x2x15
28x23x9
1x1x17
7x23x9
30x9x27
9x16x18
15x24x28
30x11x18
29x26x10
9x5x25
2x1x19
14x3x14
6x3x6
30x15x20
20x17x27
28x10x9
14x24x28
17x11x6
12x3x6
8x8x15
23x14x21
11x21x7
5x13x30
4x29x25
30x28x24
18x4x9
3x15x6
13x9x19
30x14x7
7x9x9
17x11x26
24x26x13
16x21x16
27x17x25
2x21x11
9x11x27
3x3x7
13x8x14
20x20x26
13x29x22
30x11x1
7x10x19
27x5x9
23x17x15
21x6x13
24x15x16
18x4x14
18x16x6
22x11x18
14x2x5
15x3x7
10x20x29
16x1x10
30x23x1
10x15x11
17x14x5
22x8x13
7x11x28
26x17x3
2x23x2
28x13x19
18x12x28
22x23x16
14x12x1
20x8x19
17x19x13
29x2x12
2x26x27
29x16x4
13x8x18
16x15x30
23x16x2
28x8x27
21x8x23
13x20x26
19x6x17
17x30x15
7x4x30
2x13x30
18x7x19
4x13x27
8x6x5
18x20x25
2x3x30
23x27x13
22x30x4
23x25x25
23x16x19
25x3x1
5x6x15
11x29x12
25x24x7
16x7x20
20x3x2
12x27x15
16x10x12
1x3x14
22x1x26
2x24x18
11x29x16
15x2x9
10x1x24
21x8x11
30x11x23
6x30x21
13x27x29
14x6x5
18x29x19
12x4x28
29x3x14
10x30x28
5x7x15
14x1x10
9x25x14
7x24x18
28x17x21
18x13x25
26x15x1
21x1x19
12x16x21
4x6x13
7x15x26
17x19x5
12x28x2
1x20x19
27x7x5
17x26x8
12x15x19
5x23x10
8x2x8
16x13x12
14x27x1
26x29x3
24x16x14
14x13x13
7x22x23
2x9x30
4x27x8
26x27x15
23x1x6
25x29x18
5x18x1
20x8x20
5x10x25
30x25x15
7x22x25
28x26x17
29x4x1
21x11x27
20x9x8
25x22x12
2x11x11
23x2x16
23x27x20
2x13x28
27x2x24
11x1x17
12x4x27
16x20x22
30x12x10
5x15x4
5x2x27
12x4x25
1x16x4
27x4x4
21x16x3
27x26x3
24x6x6
24x12x12
20x20x25
8x29x2
21x4x5
2x4x8
4x13x19
3x20x10
12x15x16
6x5x4
12x16x20
22x19x17
8x17x22
25x16x15
7x1x19
10x1x7
23x23x5
28x6x12
2x25x12
10x27x12
24x27x19
14x14x20
4x1x5
16x27x29
20x20x24
28x24x30
6x15x15
9x15x30
23x26x3
17x24x21
22x25x25
18x29x10
20x25x1
24x11x16
20x7x21
20x7x9
7x26x2
5x18x1
16x26x28
4x10x18
27x30x21
26x9x9
8x16x14
6x27x8
28x9x20
13x13x4
9x18x16
18x15x18
22x19x14
14x10x17
25x29x11
1x18x19
8x11x26
18x6x14
30x24x13
27x1x27
15x9x3
2x29x17
2x26x21
22x9x9
20x20x20
22x28x2
26x5x16
11x3x14
21x16x16
18x26x7
18x30x6
7x11x12
15x10x2
27x2x16
27x30x24
28x14x24
7x4x8
6x28x15
13x19x1
22x26x30
7x30x24
2x17x21
19x26x2
19x24x15
14x23x2
21x27x15
30x15x14
21x29x5
23x30x2
4x1x2
15x5x13
21x2x30
20x7x16
1x21x25
2x25x1
12x29x5
28x13x16
26x3x12
29x20x23
28x12x20
4x30x8
16x15x16
6x16x29
2x28x13
24x25x2
26x15x22
17x20x11
18x12x7
19x1x18
8x27x13
22x16x8
19x26x17
13x11x10
22x12x3
13x12x14
29x17x9
6x14x10
14x20x10
8x26x9
25x13x22
3x30x25
14x28x1
30x29x12
3x17x15
3x24x14
28x24x22
16x6x1
20x25x14
17x17x13
6x19x27
10x15x20
8x23x20
7x29x21
18x9x25
10x5x22
2x27x27
16x18x30
15x5x12
26x29x29
28x11x10
9x29x28
24x15x23
26x9x10
5x1x25
22x27x16
7x29x3
1x3x5
8x7x29
19x21x11
28x13x30
17x16x20
5x10x25
9x14x15
15x14x23
16x4x17
21x8x2
9x9x8
22x22x4
10x2x27
12x19x10
15x29x4
22x14x7
29x18x5
1x7x27
24x1x15
23x23x26
12x17x23
26x10x24
8x22x2
8x1x10
22x19x12
2x23x13
11x27x25
26x15x27
27x7x21
18x9x6
22x21x22
7x12x26
23x21x13
14x3x8
5x9x28
29x29x15
27x25x23
12x2x24
8x2x20
29x19x4
12x24x29
2x27x28
14x20x9
28x6x25
18x29x8
19x11x30
15x11x23
18x7x7
14x20x14
26x18x22
27x25x13
12x10x30
30x2x7
28x10x1
18x10x30
22x11x5
22x16x3
25x15x9
5x10x24
4x28x8
19x24x18
3x4x25
14x4x30
11x26x3
12x12x12
26x7x24
3x2x14
1x27x7
2x2x13
3x26x26
12x4x11
12x17x20
4x19x30
5x18x10
17x6x18
19x30x20
11x2x17
30x13x19
22x23x7
17x28x2
5x17x30
7x11x4
21x26x18
15x28x4
5x6x27
12x6x16
9x17x12
27x20x5
14x5x20
27x14x6
2x14x21
4x28x30
24x5x1
19x29x29
11x23x1
8x16x21
3x17x19
10x13x5
20x21x16
23x3x6
27x26x11
3x2x22
14x3x5
10x9x8
"""

  let day03input = "^><^>>>^<^v<v^^vv^><<^<><<vv^<>^<^v>^vv<>v><vv^^<>>^^^v<<vv><<^>^<^v<^>^v><<<v^<v<<<v<<vv<v<^><^>><>v>v^<<v^^<^v<><^>^<<^^^>v>>v^^<v>>^>vv><v>>^>>v^>^v>^<^^v>^>^^v<v>^^<v<>>v^^v><^><^<<>v^<^<^v<v>v^>>>v^v^>^<>^v<^^vv<v>^>^<>^^<vv^<><<v<^<^^>vv<>^>v<^>^v>v^>^v<>^><>><vv<>v^v<><>v^v>>>>v^^>^><^^<v<^><^<v>>^v^v<>v<<<^<<vvvv<<v^vv^>v^^^<^^^<v>>v<^v>>>>>v<^^^^>v<^<><v>>>>><v>>v^vvvv^^<v^<>^v<^v^>v><^>^v<<>>vv^>v>v^^>vv^<^vvv<>><>><><^^^<v<>^<^^^<v><^v>>v>^v<v^vv^<>^^^>v^^^v>>^v^^<^>>^>^<<v>>>^^<>>^vv>v^<^>>>><v<><><^^v<><<<<^^<>>^<vvv^><>v<v<<<<><v<<v>v<v^><vv<v^>^<^>v^^><^v>^^>v<>^v^<>^vv^><v^^vv>vvv>v>^<vv^>>^>>^>><>>>^^^^v<vv>^<>v^^><v^>^<>v<^^v><v<<><^v><>^^^^^v^v>>^^v><<><<vv>^^^^><^>v>><<<^v>v^^>^v^<^^v>v<^<<>>^v<<<v<<>>v<^v^><vv<v^v>v^<v>><v>^v<<<vv^>v<v>>v>>v><v><v^>v^^v>^v^>>>><>^>v>^v^>>>>v^<<vv<^v><<>v<v^<^^<<v<^v^^v^>vv><vv<v^<^>><^^>^<><^^<v<><^v^v^<^^>^<v><^<v>v^<<<^^v<v>^v>>><>^^>vv<<^v^<<<<^^>>>v>v<<<>^^>>>v>^>v>vv<<>^<^><v^>^^<^<v<<v<^>>^v^<vvv><>v^><<v>^^<v^vv^^^<vvv^<^>^>vv>><^v<^<<v<><<><<^^<><><vv>v>^<v>>^<>>^^v>vv^<^^v>><^vv^<<v^^><<>vv<v<><v<><v^^^v^v>^v<^<>v^^>><>^<^<v^<v^v^>v<<<^<<^>>>^^<^^v>v^<v>vvvv>v<>><^>^<<<<v^<v<>v^^^v<>v>^<v<<^^v^^<>^<<v^^<^<v>v>>v>>v^>^<vv<<<<<^<><>v><>>>v^>^v<^<><<v<^v^^<^<><^>^^^>^><>^><<vv>^<>vv<<v^v<<<<<>>>v<vv>^v>^>^>^<^><>v<><>>>^^<v>^<^v>>^<><v^><v^>>>v<v^^vvv^><v<v>v^>vvvv>>><^>v<>^^^>v>>v^<v<>v^>^<v^>^<<^>^>>v<<><<v^^>>v^<v^<^v^>^>v^><<^<v>v^<v>>^^<<v>v><<<^v^<>^<>^>>^<<v>^^<>^v<>v^>>><<v>><v^>^><v^<><v><>><v^<>vv>v^<^^^>v>^^<vv>>^v<><>>><>><^<>>v>v^^>^^<^^>^>>v>vv^^v<^<^v><vv<v<^>><<vvv<<><^>^v>^^^<<>v^<v<v><<v>^^v<<<>^^vv<^>vv>^>^<><<>vv<^>v^vv>^^^v><<^vv>^v<><v^^^^v^>vv^^<^<>^^v^<^vv<v<vv<>v>v^^<>^^>^^>^<><<^v>^><^^vvvv<><>^<v^^>v<>^><>v>><>vv^<<><<>><>v<^>^v>>^^v><<<>>^<^v^<v<<<v^>^^<^<><><^><<<<^<vv><v<<><vvv^^><vv>^<<vv<<<^v<>>><><>>v><<<v>vvvv^^vv<v>><<^v^vvv><><vv>v><>v<<<^<v^>><^^>v^<v>><v>^^^v^v>><<<v<^^>>^v<>v^<vv^^<<v<v>v<<<<^^^v^v<<>>>v>>vv>^^<><^v<v><>>v^>>>>>^>v^v^<^v^v^vvv>v<v<^>vv^<<v>vv>>v^^vv<^v>>>>vv<>v<>^^vv^<v>v^>>vvv<<<v<<^vv^^^^>v>v>^><<<^>v^><v<^<<<v>^v^^^><<><<<^^<^^<>^<v>^<v<<v<^^vv>v<^v><v><v<>^v<^<v<^<v^v><v>><v<v<<>^<v<>>><>^v^v<<^><v^<<v<v^>^>v><^>^vv^^<v<v<vv<v>^v^v^>^<<>>>>>v^<>^>v^vv^><<>>^^<>v^><v>^vvv^>v^v><>^><<>v>v<^<^><^^vv<<><>>v>>v><vv>>^v<<>^vv<>^vv>v>v>^>^>>><><<>v<v>^<<^v^^<<<><v>>vv<^<vv<vv^<<v<<^v><<>v<^^^<<^v^>^v>^^^v^v>>>v>v^v>^>^vv<^^<<vv^>^<<<vv>v^<><<^vvv^^><>vv^v>v>^><<^^^^vvv^<vvv>><^v<^>^<>>^<v<<vv>>><v>vv^<>><v^<v>^v>^>v>^<^<^^^<<vvvv^>>>>>>>v><vv>^<>^^v^><>><^v^^<v^v<<<<v^>><>v^v<vv<><^<<<<^>^^>vv>><^v<v^v<<>^vvv>v^^><^^<^<>>^^v^vv<>v<^<<<v^^^><v<vv<<>v>v<>^v^><v^vv^v^^v<^^v^^v><>v<^v>><<^<^v^>><<vv<<^>^<<v^<>^><>v><vv^v>>^<v<<<^>vv<^v>^>v<<v>^>>^>>v^<v<v>>^v<^v^v><<><>^><<<><v<vvvv<v^<v^v><>^<>^^^^v>^>^vvvvv>v>>v><<vv<<v<><<^><<^v><<v<<<v><vv<^>^v>>>>^v<^v<<>>^>^<<vv^<^>v>><<^>^>^v><><>^><<v<>v^><<^v^<^^><^^v^<<^v^^>>^v^<^><vv>v^^<<^^^<><>^>v^v>v^>^v^vv>^^>>>>^^<^>>>^^v<vv<><^^<vvv<^^^vv>v<v<v>><<<>^>^^>^>^v<<<<>>^<<>><v>>v>^^<^v<>v<>v^>v^><^<^^><v^^v>^^vv<v<<>><<vv<>>v>^<<<<v<<v>^><^^<^<^<v^<<^^v>^v<^>v^v^<v^vv^>^^><^>v^v>>^^v^><vv<v<v<v>>>>><<><v><v^v^<v^<^^<v<>^>v>v<>>>v>^^^^>><v^v^^v<<<>v^<<^<v>>>><^v^<<><v<>>v><><v<v^v>^v^^<v<^<^^v>><<vv<<vv><>>^>^>vv<^<>^vvv^v<v^^<>v^v>^^<<<<<>^v^>^<>v^^<>v^v<vv>^<>vv^<^vv>><v^^vvvvv>><<>v<vv^<^<vv^v^<>^^<v^<vv^<v^v^v<<^>^>^>^^>>>vvv>^>v>v>>>^>vv^><>^><>v>^^<v^>^><<v>><<<>>v<vvvv^>^v<^<>^<v>^<>^^<<><>^v<><>>>^vv<^<<^<^v>v<<<<<^^v<^v<><v<<><^>v>^v>>^v^><^^^^v<><><>vv^<>vv<^v<^^><v^<^><^^v^v^<^^<<><v>v<v<v^<<^v><>v^v<^>vvv><<^v>>v><><v<<^>>>v<^>>v>^<>><>^<v^v^<vv<<^>v<^^>^<^v<^<<^^v<>>^>^>^v^^v^v<v^^vv^<v>>v><vv^vv>v<>v^>v^^>^^>><v><v^<<><<>><<^^>><^v<v<><<><<><v<v^<^<v>>>><v^^v^^>>>^^^^^<<vv<^><>^<<<vv^^^>^><<<v<^v>^<v<^>^vvv<<>vv><<>v>v^v>>>>>^<>><^^^><<<<v><<vv>>>v<^<vv^v^<<v>>>>^^vvv>v<>><v>>>v>>^v^vvv<<>vvv<<^^^<>vv^^v<<>^^^>>^<^v^<^^>v^><v>>^<<^v<<vv<vv>v^>>^>v^><^><>^>>>vv>><^^^>vv<<^^vv><^<>^>^^<^<>>^vv^>>^v><>v^>>><<<^^<^>^>v<^>^<^^<>>><^^<>^v^<<vvv<v><>vvv><v>v^v<<^<v>^^><<^vv^v>v>v<<^v^<<<>^><><vvv>v>^vv^v<>vv^>^^<^>^>v^^<vv^>v><v<<<><>>^v<^<><><^<v^^<<^<v>vv<><<>v^<v^>^>^^<><<>^<^<<v^^v<v^<><<>v>><^<<>^>^v^v<v^v><^>>^v<^>v<<>^^^<^v>>>^<v>vvvv<<v^<^^>vvvv>v<>v<v><vvvvv>^<><>vvv<>^<<>^>>>>v^<^<><^v>v^>>v><>^><<v^>^<<>^>^v^<v^^>>^v><v>^<v><>v^<^^>v>^>>>v^v>>>^<>^<>>>>>v>>vv^v<><<<><><v><<vv<<v<><>>vv<^<vv>^v<<>v^v<^v<><v>>^v>>vvv^^v>>v>^>^>v><v><^>^^<<>^v<^<<<<^>v<^>>v^<^v>^v<<>^>^vvv<^^vv>^vv>vv<>>v>v<v>>v^<<<<<^^v^>v>^<<<v^v>>v<v><vvv><v>^<vv><<>>^<^>^^<>>>>^<^v<>v^^>^<^^v<^><>><v>>^v^vv<^v<^><<vvv<>><>><^^>^<^v^<^<>v<<<^v>v^^^<>v^<v^>^v^>><>^^<v<^><<^^v^<>^<^vv>>><^v><v^>vv<^v<<<v^>>v>v^v>^<v>v<^<>v^vvv>^vv<<<<v><^><v>>^^>><^v><<^>v^^<<v^^<^<><<<<>^<v<^v^>v<<^^>v<<<<<vvv<v<^>^>^>^>>^>>>v^<<v>>^^v><vv<^v<v<^^^>>>^vvv<^v<>>>vv>^^><^v>vv^>>v>v^<>^<vv>^>^<<^>^^^>>^vv>^^>vvvv<>>^^^^>>>v>v^^>vv>vv^<<>^><^<v^vvvv><v<><v>><<<v<v<<^v><vv^vv^<>>>^>^<v<^v<>><^<vv^^><v>v^>v^<><v^vvv>^>v^^v^>^^>v<<<<^<<^>>v>v^^^<<<v>>>^^v>v<v><<<<^^^v>^vv^>><>^v<v<<^^<<<<><>>>v>vvv^v^^v^>>vv>^>><>^v><^v^><^^>vv>^<^<^>><v>v>><><><v>^>^>v>vv>vv>^^>v>v^><v<<v^<>^>^v>^^v>^<^v<>>vvv^^>^>vv<v<v<<^<^<v^<>v^^v<^<^>vv^^<v><^^^>v>vv<<v>v<<v^<v^^><vv>^>^v^<^>v<^>^<>vv^><v<^><>>^>>^<^><<>^<^>v>v><>>>^<<^><<v><^v<v><>>vv<^><v^>>v>v>>>>^^>v<^v^>><<^<>>v><^><<^>^<vv^^<><<>><vvvv^>^^<><^^v>^^>vv>^v<v>>^^v^<v<^><^<<>>v^^^<^><^<<><<v<>><<>^v>vvv^vvv^^>>^<^<v>><>^<<<<^^<>>>v^<<^^v>><><<v<^>v>^v<v^>v>vv^><>^><<><^^>^>^<><>><^^<v^v<^><><><v>^<v<<v^<<^^^v<v<^v<>>><^v<<<<>>^v>^^vv^v^<<v>><<<v>vv>>v>>^v^<>>vv^<^>^<<>v<<<^vv<^vv^vv<^v^^^<vv^>v>>v<^^<^^vvv<^^v<>>>^>v^><v>^^><>vv>v>v<<<^^v<^vv^v>^^^>>>^^<>^^<^vvv>><><<><^<v>><<>^>^^<v^v^>vv>vv<v>^^<^^<<><><<v><v^^>v><v><<>v>vvv<^^^^<^>>><<<^^^<^>vv^^v>>v<<v^^<vv^<^>vvv^^v^^<^<vv>v<^<>^<<vv^^>^v>>^><><>v<v<v<>><v>>>^^>>v^><v^^<^>><>v<><<v^v<v<<>>>><>>>>><<^vvv<<><><<>^><><<^^v><<^>v>^>^v>v>>^^<><^>vv<^<^v>v<><^<<v<><^><>^^^<v^<><vvv^^^<>^^v><v<<<v>><>^>^vv<v^<vv>v>v^vv<v^v<v>^v^>v><>v^><>v>^^^^><<vv^><v<<v<^<>^v^^^>^^><<<v<^<v^>^^>v><vvvvv^<^<v^^>v<^v^^vv^<<<<v><^>v>v^v><><v^<<^<<v<^^^>^><v^v^<><><>^v<v>^<>^v>^v>v^<><^><v>>v<<^><^vv^<><^<>><>><v<v><<^^^^>v<^<^vv<><^vv><<^<<v>v^>>^v>^>v^^v>vv<v>v<<v>v<>^>>vv^>>><>^v^^<^>v<<^<^^v^^v^<<v<<v<^v<>vv^<v>><^v<^>>>vv^^<v^<>^^v<v<v>>^><^^^<><<^^>v<<vv>><<vvv>><<v^v^>><>vv^><<^>^><^v<^<^<vv<^^vv>v^v<<<<<<><<vv^vv>vv>v<^><<><><<>>v>><v><^>^v>^v^<>v^^^><^^<<<^vv^vv>^v^vvv^^>v^<v>><^<^<^<>^vv<vv^v^^>^^^>vv^v>>><<<^<>>v>v<^^<><v>>><><^v^^<<><<<>^<^^v^>v<vv^^^^>><v><^<<v<<v<>^>^>>^<>^v><>>^<v<vv^<<^<<>vv^>^^<<<^v<>>^v<>vvv<<^^<<><vvvvv<<^<^^<>>>>^^<><>^><>^v<v^^v<<v^^<^<^>v<v>^v<^>^v<>v^vv<><<v>^vvv<><<^>>^^><><>^<>^>v^^v^><v<><>>v><v^<v<<v>><^v>^<v<^>v<<<>vvv^<^^v<vvv^vv<>^<>^>>v<>^^><><v>>^><^^vv>><<>><v><^><>>^vv>v<vv<>v^v^^v<<^^<vv>v^^vv<<^<<><>^<><v^><^<^<>>^vv<v>v>>^<^vv>^vv^>v>^<><^><^<>v^v^^<^<>^^v>>><^v<>v^v<<^>v><>^^<<v^v<>v^>>v>^<><vv^v<v^<vv<>^>^>^<^>v><<><><><<<>^>><v^^><^>><v>>^v<<<^<<>^><<^>>>>>v<^>v>>v^<v^>^>v^^><>v^v^vvvv<v<v<>v>>><<>^<<vvv><v^v^>v<v^^^>>^<v>>^vv^^<vv><^>>v<v^><vvv<^^>>vv^v<^<>^v^<<v>^<<><<<^vvv^>^^<<>>><v<^>vv<<^<><^v<^<><<^^>vv^v>v^^^>>>>^>vv<<v>v>>^^v^^><>v<<^><^<v^>>^>v^v>><^v^>v<<^<v><^<^<^<>>v^^>><<<>v<v>v<^^>^vv<<<^^<v<>v^^>v<<><^<>^^>^v<>v>><^^^vv^>^><>v^^<v^<>>^<v^^^><v<><vvv>v>^<<^v>^>>>>><^^^<>v<v>>v^^<^v^>>v^<<v^>^>v^v>>>>^>>vv<>^<^v><v^^<>v>v^v>^<>^>v<vv><<v<^v<<^v<<^v^vv<><>^<>>^<>>^<>v^><<>^v>>^^^^<<^v><>^<^>^^v><^^<^<v^<^^v>^v><vv>v<<^>^>><<^^^vvv<<^vv<^^>v^^vv^<^^<<^^>>^^<vv<v<<v^^<<v<^vvv<<><<v>v^>>v^^>v<^>^><v<^>v<v^v<v^^<>v>><<v^v^v<^^^><v>v><^<^vv>^^v>^>v<<^vv><^^^^^^><<^>>>^v<>^^v<<<>><<<v^><>^<<<v>v^>^^^<^><v>^^^v<<>v<v>^<v^>><<^^<<^v<<>^v>>vv>><v<^><v<<<vvv><vv><<^v^^<v^vvv<^v>>v^v<v^v^>>^^v<><^^^<^^>v>^<><v<<v^^>vvv^v^^<v<v^v>^>v^^v<^><v^^<<<<>^^>>^v<><^><^<<^vv^<><<>v^vv^<v^<><<<^^>v<<>>>v<>v<><<<v>^v>^^v>^^>v>^>^>v<>><>^>^>^vvvv<^<v^<>^^^^v>v>><<v>>^<vv>>^<v<^v^vv>><>^^>v^^<<><^<v>><<<<>v>^^><v^^v<<v<><vv^v>^<v^^>v<<<<v^v<<>>vv<v<<<v>v>>v<^v>>v>v^<<<>^>^>^<>v<^^vv><^v<<^v<vvv^vv>v<^<<^^vv^^>vv<^>v>^^<<v^<<^^v<>^>v<<^^<^>^^^v^^<v<^<^>>>v^vv^<^v>^<>^<^<v<^v>>>^<^v<><v<^vv<v>v><v^v^^v<vv><^^<><>^>v<^<^vv>><^v><v<>^<>^^>^<><<<v^>>^<>><<><v>vvv^<<^<vv<v><v<^<<<^>^>>v<^>>vv>^v^^^v<>v<>><>^vv^>vv^"

  let day05input = """
sszojmmrrkwuftyv
isaljhemltsdzlum
fujcyucsrxgatisb
qiqqlmcgnhzparyg
oijbmduquhfactbc
jqzuvtggpdqcekgk
zwqadogmpjmmxijf
uilzxjythsqhwndh
gtssqejjknzkkpvw
wrggegukhhatygfi
vhtcgqzerxonhsye
tedlwzdjfppbmtdx
iuvrelxiapllaxbg
feybgiimfthtplui
qxmmcnirvkzfrjwd
vfarmltinsriqxpu
oanqfyqirkraesfq
xilodxfuxphuiiii
yukhnchvjkfwcbiq
bdaibcbzeuxqplop
ivegnnpbiyxqsion
ybahkbzpditgwdgt
dmebdomwabxgtctu
ibtvimgfaeonknoh
jsqraroxudetmfyw
dqdbcwtpintfcvuz
tiyphjunlxddenpj
fgqwjgntxagidhah
nwenhxmakxqkeehg
zdoheaxqpcnlhnen
tfetfqojqcdzlpbm
qpnxkuldeiituggg
xwttlbdwxohahwar
hjkwzadmtrkegzye
koksqrqcfwcaxeof
wulwmrptktliyxeq
gyufbedqhhyqgqzj
txpunzodohikzlmj
jloqfuejfkemcrvu
amnflshcheuddqtc
pdvcsduggcogbiia
yrioavgfmeafjpcz
uyhbtmbutozzqfvq
mwhgfwsgyuwcdzik
auqylgxhmullxpaa
lgelzivplaeoivzh
uyvcepielfcmswoa
qhirixgwkkccuzlp
zoonniyosmkeejfg
iayfetpixkedyana
ictqeyzyqswdskiy
ejsgqteafvmorwxe
lhaiqrlqqwfbrqdx
ydjyboqwhfpqfydc
dwhttezyanrnbybv
edgzkqeqkyojowvr
rmjfdwsqamjqehdq
ozminkgnkwqctrxz
bztjhxpjthchhfcd
vrtioawyxkivrpiq
dpbcsznkpkaaclyy
vpoypksymdwttpvz
hhdlruwclartkyap
bqkrcbrksbzcggbo
jerbbbnxlwfvlaiw
dwkasufidwjrjfbf
kkfxtjhbnmqbmfwf
vmnfziwqxmioukmj
rqxvcultipkecdtu
fhmfdibhtjzkiqsd
hdpjbuzzbyafqrpd
emszboysjuvwwvts
msyigmwcuybfiooq
druyksfnbluvnwoh
fvgstvynnfbvxhsx
bmzalvducnqtuune
lzwkzfzttsvpllei
olmplpvjamynfyfd
padcwfkhystsvyfb
wjhbvxkwtbfqdilb
hruaqjwphonnterf
bufjobjtvxtzjpmj
oiedrjvmlbtwyyuy
sgiemafwfztwsyju
nsoqqfudrtwszyqf
vonbxquiiwxnazyl
yvnmjxtptujwqudn
rrnybqhvrcgwvrkq
taktoxzgotzxntfu
quffzywzpxyaepxa
rfvjebfiddcfgmwv
iaeozntougqwnzoh
scdqyrhoqmljhoil
bfmqticltmfhxwld
brbuktbyqlyfpsdl
oidnyhjkeqenjlhd
kujsaiqojopvrygg
vebzobmdbzvjnjtk
uunoygzqjopwgmbg
piljqxgicjzgifso
ikgptwcjzywswqnw
pujqsixoisvhdvwi
trtuxbgigogfsbbk
mplstsqclhhdyaqk
gzcwflvmstogdpvo
tfjywbkmimyyqcjd
gijutvhruqcsiznq
ibxkhjvzzxgavkha
btnxeqvznkxjsgmq
tjgofgauxaelmjoq
sokshvyhlkxerjrv
ltogbivktqmtezta
uduwytzvqvfluyuf
msuckpthtgzhdxan
fqmcglidvhvpirzr
gwztkqpcwnutvfga
bsjfgsrntdhlpqbx
xloczbqybxmiopwt
orvevzyjliomkkgu
mzjbhmfjjvaziget
tlsdxuhwdmghdyjb
atoecyjhwmznaewi
pyxpyvvipbqibiox
ajbfmpqqobfsmesj
siknbzefjblnohgd
eqfhgewbblwdfkmc
opylbscrotckkrbk
lbwxbofgjkzdxkle
ceixfjstaptdomvm
hnkrqxifjmmjktie
aqykzeuzvvetoygd
fouahjimfcisxima
prkzhutbqsyrhjzx
qqwliakathnsbzne
sayhgqtlcqqidqhj
ygduolbysehdudra
zricvxhdzznuxuce
ucvzakslykpgsixd
udirhgcttmyspgsb
yuwzppjzfsjhhdzi
gtqergjiuwookwre
xvxexbjyjkxovvwf
mlpaqhnnkqxrmwmm
ezuqbrjozwuqafhb
mcarusdthcbsonoq
weeguqeheeiigrue
pngtfugozxofaqxv
copphvbjcmfspenv
jiyahihykjjkdaya
gdqnmesvptuyrfwp
vbdscfywqmfxbohh
crtrfuxyjypzubrg
seihvevtxywxhflp
fvvpmgttnapklwou
qmqaqsajmqwhetpk
zetxvrgjmblxvakr
kpvwblrizaabmnhz
mwpvvzaaicntrkcp
clqyjiegtdsswqfm
ymrcnqgcpldgfwtm
nzyqpdenetncgnwq
cmkzevgacnmdkqro
kzfdsnamjqbeirhi
kpxrvgvvxapqlued
rzskbnfobevzrtqu
vjoahbfwtydugzap
ykbbldkoijlvicbl
mfdmroiztsgjlasb
quoigfyxwtwprmdr
ekxjqafwudgwfqjm
obtvyjkiycxfcdpb
lhoihfnbuqelthof
eydwzitgxryktddt
rxsihfybacnpoyny
bsncccxlplqgygtw
rvmlaudsifnzhcqh
huxwsyjyebckcsnn
gtuqzyihwhqvjtes
zreeyomtngvztveq
nwddzjingsarhkxb
nuqxqtctpoldrlsh
wkvnrwqgjooovhpf
kwgueyiyffudtbyg
tpkzapnjxefqnmew
ludwccvkihagvxal
lfdtzhfadvabghna
njqmlsnrkcfhtvbb
cajzbqleghhnlgap
vmitdcozzvqvzatp
eelzefwqwjiywbcz
uyztcuptfqvymjpi
aorhnrpkjqqtgnfo
lfrxfdrduoeqmwwp
vszpjvbctblplinh
zexhadgpqfifcqrz
ueirfnshekpemqua
qfremlntihbwabtb
nwznunammfexltjc
zkyieokaaogjehwt
vlrxgkpclzeslqkq
xrqrwfsuacywczhs
olghlnfjdiwgdbqc
difnlxnedpqcsrdf
dgpuhiisybjpidsj
vlwmwrikmitmoxbt
sazpcmcnviynoktm
pratafauetiknhln
ilgteekhzwlsfwcn
ywvwhrwhkaubvkbl
qlaxivzwxyhvrxcf
hbtlwjdriizqvjfb
nrmsononytuwslsa
mpxqgdthpoipyhjc
mcdiwmiqeidwcglk
vfbaeavmjjemfrmo
qzcbzmisnynzibrc
shzmpgxhehhcejhb
wirtjadsqzydtyxd
qjlrnjfokkqvnpue
dxawdvjntlbxtuqc
wttfmnrievfestog
eamjfvsjhvzzaobg
pbvfcwzjgxahlrag
omvmjkqqnobvnzkn
lcwmeibxhhlxnkzv
uiaeroqfbvlazegs
twniyldyuonfyzqw
wgjkmsbwgfotdabi
hnomamxoxvrzvtew
ycrcfavikkrxxfgw
isieyodknagzhaxy
mgzdqwikzullzyco
mumezgtxjrrejtrs
nwmwjcgrqiwgfqel
wjgxmebfmyjnxyyp
durpspyljdykvzxf
zuslbrpooyetgafh
kuzrhcjwbdouhyme
wyxuvbciodscbvfm
kbnpvuqwmxwfqtqe
zddzercqogdpxmft
sigrdchxtgavzzjh
lznjolnorbuddgcs
ycnqabxlcajagwbt
bnaudeaexahdgxsj
rlnykxvoctfwanms
jngyetkoplrstfzt
tdpxknwacksotdub
yutqgssfoptvizgr
lzmqnxeqjfnsxmsa
iqpgfsfmukovsdgu
qywreehbidowtjyz
iozamtgusdctvnkw
ielmujhtmynlwcfd
hzxnhtbnmmejlkyf
ftbslbzmiqkzebtd
bcwdqgiiizmohack
dqhfkzeddjzbdlxu
mxopokqffisxosci
vciatxhtuechbylk
khtkhcvelidjdena
blatarwzfqcapkdt
elamngegnczctcck
xeicefdbwrxhuxuf
sawvdhjoeahlgcdr
kmdcimzsfkdfpnir
axjayzqlosrduajb
mfhzreuzzumvoggr
iqlbkbhrkptquldb
xcvztvlshiefuhgb
pkvwyqmyoazocrio
ajsxkdnerbmhyxaj
tudibgsbnpnizvsi
cxuiydkgdccrqvkh
cyztpjesdzmbcpot
nnazphxpanegwitx
uphymczbmjalmsct
yyxiwnlrogyzwqmg
gmqwnahjvvdyhnfa
utolskxpuoheugyl
mseszdhyzoyavepd
ycqknvbuvcjfgmlc
sknrxhxbfpvpeorn
zqxqjetooqcodwml
sesylkpvbndrdhsy
fryuxvjnsvnjrxlw
mfxusewqurscujnu
mbitdjjtgzchvkfv
ozwlyxtaalxofovd
wdqcduaykxbunpie
rlnhykxiraileysk
wgoqfrygttlamobg
kflxzgxvcblkpsbz
tmkisflhativzhde
owsdrfgkaamogjzd
gaupjkvkzavhfnes
wknkurddcknbdleg
lltviwincmbtduap
qwzvspgbcksyzzmb
ydzzkumecryfjgnk
jzvmwgjutxoysaam
icrwpyhxllbardkr
jdopyntshmvltrve
afgkigxcuvmdbqou
mfzzudntmvuyhjzt
duxhgtwafcgrpihc
tsnhrkvponudumeb
sqtvnbeiigdzbjgv
eczmkqwvnsrracuo
mhehsgqwiczaiaxv
kaudmfvifovrimpd
lupikgivechdbwfr
mwaaysrndiutuiqx
aacuiiwgaannunmm
tjqjbftaqitukwzp
lrcqyskykbjpaekn
lirrvofbcqpjzxmr
jurorvzpplyelfml
qonbllojmloykjqe
sllkzqujfnbauuqp
auexjwsvphvikali
usuelbssqmbrkxyc
wyuokkfjexikptvv
wmfedauwjgbrgytl
sfwvtlzzebxzmuvw
rdhqxuechjsjcvaf
kpavhqkukugocsxu
ovnjtumxowbxduts
zgerpjufauptxgat
pevvnzjfwhjxdoxq
pmmfwxajgfziszcs
difmeqvaghuitjhs
icpwjbzcmlcterwm
ngqpvhajttxuegyh
mosjlqswdngwqsmi
frlvgpxrjolgodlu
eazwgrpcxjgoszeg
bbtsthgkjrpkiiyk
tjonoglufuvsvabe
xhkbcrofytmbzrtk
kqftfzdmpbxjynps
kmeqpocbnikdtfyv
qjjymgqxhnjwxxhp
dmgicrhgbngdtmjt
zdxrhdhbdutlawnc
afvoekuhdboxghvx
hiipezngkqcnihty
bbmqgheidenweeov
suprgwxgxwfsgjnx
adeagikyamgqphrj
zzifqinoeqaorjxg
adhgppljizpaxzld
lvxyieypvvuqjiyc
nljoakatwwwoovzn
fcrkfxclcacshhmx
ownnxqtdhqbgthch
lmfylrcdmdkgpwnj
hlwjfbvlswbzpbjr
mkofhdtljdetcyvp
synyxhifbetzarpo
agnggugngadrcxoc
uhttadmdmhidpyjw
ohfwjfhunalbubpr
pzkkkkwrlvxiuysn
kmidbxmyzkjrwjhu
egtitdydwjxmajnw
civoeoiuwtwgbqqs
dfptsguzfinqoslk
tdfvkreormspprer
zvnvbrmthatzztwi
ffkyddccrrfikjde
hrrmraevdnztiwff
qaeygykcpbtjwjbr
purwhitkmrtybslh
qzziznlswjaussel
dfcxkvdpqccdqqxj
tuotforulrrytgyn
gmtgfofgucjywkev
wkyoxudvdkbgpwhd
qbvktvfvipftztnn
otckgmojziezmojb
inxhvzbtgkjxflay
qvxapbiatuudseno
krpvqosbesnjntut
oqeukkgjsfuqkjbb
prcjnyymnqwqksiz
vuortvjxgckresko
orqlyobvkuwgathr
qnpyxlnazyfuijox
zwlblfkoklqmqzkw
hmwurwtpwnrcsanl
jzvxohuakopuzgpf
sfcpnxrviphhvxmx
qtwdeadudtqhbely
dbmkmloasqphnlgj
olylnjtkxgrubmtk
nxsdbqjuvwrrdbpq
wbabpirnpcsmpipw
hjnkyiuxpqrlvims
enzpntcjnxdpuqch
vvvqhlstzcizyimn
triozhqndbttglhv
fukvgteitwaagpzx
uhcvukfbmrvskpen
tizcyupztftzxdmt
vtkpnbpdzsaluczz
wodfoyhoekidxttm
otqocljrmwfqbxzu
linfbsnfvixlwykn
vxsluutrwskslnye
zbshygtwugixjvsi
zdcqwxvwytmzhvoo
wrseozkkcyctrmei
fblgtvogvkpqzxiy
opueqnuyngegbtnf
qxbovietpacqqxok
zacrdrrkohfygddn
gbnnvjqmkdupwzpq
qgrgmsxeotozvcak
hnppukzvzfmlokid
dzbheurndscrrtcl
wbgdkadtszebbrcw
fdmzppzphhpzyuiz
bukomunhrjrypohj
ohodhelegxootqbj
rsplgzarlrknqjyh
punjjwpsxnhpzgvu
djdfahypfjvpvibm
mlgrqsmhaozatsvy
xwktrgyuhqiquxgn
wvfaoolwtkbrisvf
plttjdmguxjwmeqr
zlvvbwvlhauyjykw
cigwkbyjhmepikej
masmylenrusgtyxs
hviqzufwyetyznze
nzqfuhrooswxxhus
pdbdetaqcrqzzwxf
oehmvziiqwkzhzib
icgpyrukiokmytoy
ooixfvwtiafnwkce
rvnmgqggpjopkihs
wywualssrmaqigqk
pdbvflnwfswsrirl
jeaezptokkccpbuj
mbdwjntysntsaaby
ldlgcawkzcwuxzpz
lwktbgrzswbsweht
ecspepmzarzmgpjm
qmfyvulkmkxjncai
izftypvwngiukrns
zgmnyjfeqffbooww
nyrkhggnprhedows
yykzzrjmlevgffah
mavaemfxhlfejfki
cmegmfjbkvpncqwf
zxidlodrezztcrij
fseasudpgvgnysjv
fupcimjupywzpqzp
iqhgokavirrcvyys
wjmkcareucnmfhui
nftflsqnkgjaexhq
mgklahzlcbapntgw
kfbmeavfxtppnrxn
nuhyvhknlufdynvn
nviogjxbluwrcoec
tyozixxxaqiuvoys
kgwlvmvgtsvxojpr
moeektyhyonfdhrb
kahvevmmfsmiiqex
xcywnqzcdqtvhiwd
fnievhiyltbvtvem
jlmndqufirwgtdxd
muypbfttoeelsnbs
rypxzbnujitfwkou
ubmmjbznskildeoj
ofnmizdeicrmkjxp
rekvectjbmdnfcib
yohrojuvdexbctdh
gwfnfdeibynzjmhz
jfznhfcqdwlpjull
scrinzycfhwkmmso
mskutzossrwoqqsi
rygoebkzgyzushhr
jpjqiycflqkexemx
arbufysjqmgaapnl
dbjerflevtgweeoj
snybnnjlmwjvhois
fszuzplntraprmbj
mkvaatolvuggikvg
zpuzuqygoxesnuyc
wnpxvmxvllxalulm
eivuuafkvudeouwy
rvzckdyixetfuehr
qgmnicdoqhveahyx
miawwngyymshjmpj
pvckyoncpqeqkbmx
llninfenrfjqxurv
kzbjnlgsqjfuzqtp
rveqcmxomvpjcwte
bzotkawzbopkosnx
ktqvpiribpypaymu
wvlzkivbukhnvram
uohntlcoguvjqqdo
ajlsiksjrcnzepkt
xsqatbldqcykwusd
ihbivgzrwpmowkop
vfayesfojmibkjpb
uaqbnijtrhvqxjtb
hhovshsfmvkvymba
jerwmyxrfeyvxcgg
hncafjwrlvdcupma
qyvigggxfylbbrzt
hiiixcyohmvnkpgk
mmitpwopgxuftdfu
iaxderqpceboixoa
zodfmjhuzhnsqfcb
sthtcbadrclrazsi
bkkkkcwegvypbrio
wmpcofuvzemunlhj
gqwebiifvqoeynro
juupusqdsvxcpsgv
rbhdfhthxelolyse
kjimpwnjfrqlqhhz
rcuigrjzarzpjgfq
htxcejfyzhydinks
sxucpdxhvqjxxjwf
omsznfcimbcwaxal
gufmtdlhgrsvcosb
bssshaqujtmluerz
uukotwjkstgwijtr
kbqkneobbrdogrxk
ljqopjcjmelgrakz
rwtfnvnzryujwkfb
dedjjbrndqnilbeh
nzinsxnpptzagwlb
lwqanydfirhnhkxy
hrjuzfumbvfccxno
okismsadkbseumnp
sfkmiaiwlktxqvwa
hauwpjjwowbunbjj
nowkofejwvutcnui
bqzzppwoslaeixro
urpfgufwbtzenkpj
xgeszvuqwxeykhef
yxoldvkyuikwqyeq
onbbhxrnmohzskgg
qcikuxakrqeugpoa
lnudcqbtyzhlpers
nxduvwfrgzaailgl
xniuwvxufzxjjrwz
ljwithcqmgvntjdj
awkftfagrfzywkhs
uedtpzxyubeveuek
bhcqdwidbjkqqhzl
iyneqjdmlhowwzxx
kvshzltcrrururty
zgfpiwajegwezupo
tkrvyanujjwmyyri
ercsefuihcmoaiep
ienjrxpmetinvbos
jnwfutjbgenlipzq
bgohjmrptfuamzbz
rtsyamajrhxbcncw
tfjdssnmztvbnscs
bgaychdlmchngqlp
kfjljiobynhwfkjo
owtdxzcpqleftbvn
ltjtimxwstvzwzjj
wbrvjjjajuombokf
zblpbpuaqbkvsxye
gwgdtbpnlhyqspdi
abipqjihjqfofmkx
nlqymnuvjpvvgova
avngotmhodpoufzn
qmdyivtzitnrjuae
xfwjmqtqdljuerxi
csuellnlcyqaaamq
slqyrcurcyuoxquo
dcjmxyzbzpohzprl
uqfnmjwniyqgsowb
rbmxpqoblyxdocqc
ebjclrdbqjhladem
ainnfhxnsgwqnmyo
eyytjjwhvodtzquf
iabjgmbbhilrcyyp
pqfnehkivuelyccc
xgjbyhfgmtseiimt
jwxyqhdbjiqqqeyy
gxsbrncqkmvaryln
vhjisxjkinaejytk
seexagcdmaedpcvh
lvudfgrcpjxzdpvd
fxtegyrqjzhmqean
dnoiseraqcoossmc
nwrhmwwbykvwmgep
udmzskejvizmtlce
hbzvqhvudfdlegaa
cghmlfqejbxewskv
bntcmjqfwomtbwsb
qezhowyopjdyhzng
todzsocdkgfxanbz
zgjkssrjlwxuhwbk
eibzljqsieriyrzr
wamxvzqyycrxotjp
epzvfkispwqynadu
dwlpfhtrafrxlyie
qhgzujhgdruowoug
girstvkahaemmxvh
baitcrqmxhazyhbl
xyanqcchbhkajdmc
gfvjmmcgfhvgnfdq
tdfdbslwncbnkzyz
jojuselkpmnnbcbb
hatdslkgxtqpmavj
dvelfeddvgjcyxkj
gnsofhkfepgwltse
mdngnobasfpewlno
qssnbcyjgmkyuoga
glvcmmjytmprqwvn
gwrixumjbcdffsdl
lozravlzvfqtsuiq
sicaflbqdxbmdlch
inwfjkyyqbwpmqlq
cuvszfotxywuzhzi
igfxyoaacoarlvay
ucjfhgdmnjvgvuni
rvvkzjsytqgiposh
jduinhjjntrmqroz
yparkxbgsfnueyll
lyeqqeisxzfsqzuj
woncskbibjnumydm
lltucklragtjmxtl
ubiyvmyhlesfxotj
uecjseeicldqrqww
xxlxkbcthufnjbnm
lhqijovvhlffpxga
fzdgqpzijitlogjz
efzzjqvwphomxdpd
jvgzvuyzobeazssc
hejfycgxywfjgbfw
yhjjmvkqfbnbliks
sffvfyywtlntsdsz
dwmxqudvxqdenrur
asnukgppdemxrzaz
nwqfnumblwvdpphx
kqsmkkspqvxzuket
cpnraovljzqiquaz
qrzgrdlyyzbyykhg
opoahcbiydyhsmqe
hjknnfdauidjeydr
hczdjjlygoezadow
rtflowzqycimllfv
sfsrgrerzlnychhq
bpahuvlblcolpjmj
albgnjkgmcrlaicl
pijyqdhfxpaxzdex
eeymiddvcwkpbpux
rqwkqoabywgggnln
vckbollyhgbgmgwh
ylzlgvnuvpynybkm
hpmbxtpfosbsjixt
ocebeihnhvkhjfqz
tvctyxoujdgwayze
efvhwxtuhapqxjen
rusksgefyidldmpo
nkmtjvddfmhirmzz
whvtsuadwofzmvrt
iiwjqvsdxudhdzzk
gucirgxaxgcassyo
rmhfasfzexeykwmr
hynlxcvsbgosjbis
huregszrcaocueen
pifezpoolrnbdqtv
unatnixzvdbqeyox
xtawlpduxgacchfe
bdvdbflqfphndduf
xtdsnjnmzccfptyt
nkhsdkhqtzqbphhg
aqcubmfkczlaxiyb
moziflxpsfubucmv
srdgnnjtfehiimqx
pwfalehdfyykrohf
sysxssmvewyfjrve
brsemdzosgqvvlxe
bimbjoshuvflkiat
hkgjasmljkpkwwku
sbnmwjvodygobpqc
bbbqycejueruihhd
corawswvlvneipyc
gcyhknmwsczcxedh
kppakbffdhntmcqp
ynulzwkfaemkcefp
pyroowjekeurlbii
iwksighrswdcnmxf
glokrdmugreygnsg
xkmvvumnfzckryop
aesviofpufygschi
csloawlirnegsssq
fkqdqqmlzuxbkzbc
uzlhzcfenxdfjdzp
poaaidrktteusvyf
zrlyfzmjzfvivcfr
qwjulskbniitgqtx
gjeszjksbfsuejki
vczdejdbfixbduaq
knjdrjthitjxluth
jweydeginrnicirl
bottrfgccqhyycsl
eiquffofoadmbuhk
lbqfutmzoksscswf
xfmdvnvfcnzjprba
uvugkjbkhlaoxmyx
wadlgtpczgvcaqqv
inzrszbtossflsxk
dbzbtashaartczrj
qbjiqpccefcfkvod
hluujmokjywotvzy
thwlliksfztcmwzh
arahybspdaqdexrq
nuojrmsgyipdvwyx
hnajdwjwmzattvst
sulcgaxezkprjbgu
rjowuugwdpkjtypw
oeugzwuhnrgiaqga
wvxnyymwftfoswij
pqxklzkjpcqscvde
tuymjzknntekglqj
odteewktugcwlhln
exsptotlfecmgehc
eeswfcijtvzgrqel
vjhrkiwmunuiwqau
zhlixepkeijoemne
pavfsmwesuvebzdd
jzovbklnngfdmyws
nbajyohtzfeoiixz
ciozmhrsjzrwxvhz
gwucrxieqbaqfjuv
uayrxrltnohexawc
flmrbhwsfbcquffm
gjyabmngkitawlxc
rwwtggvaygfbovhg
xquiegaisynictjq
oudzwuhexrwwdbyy
lengxmguyrwhrebb
uklxpglldbgqsjls
dbmvlfeyguydfsxq
zspdwdqcrmtmdtsc
mqfnzwbfqlauvrgc
amcrkzptgacywvhv
ndxmskrwrqysrndf
mwjyhsufeqhwisju
srlrukoaenyevykt
tnpjtpwawrxbikct
geczalxmgxejulcv
tvkcbqdhmuwcxqci
tiovluvwezwwgaox
zrjhtbgajkjqzmfo
vcrywduwsklepirs
lofequdigsszuioy
wxsdzomkjqymlzat
iabaczqtrfbmypuy
ibdlmudbajikcncr
rqcvkzsbwmavdwnv
ypxoyjelhllhbeog
fdnszbkezyjbttbg
uxnhrldastpdjkdz
xfrjbehtxnlyzcka
omjyfhbibqwgcpbv
eguucnoxaoprszmp
xfpypldgcmcllyzz
aypnmgqjxjqceelv
mgzharymejlafvgf
tzowgwsubbaigdok
ilsehjqpcjwmylxc
pfmouwntfhfnmrwk
csgokybgdqwnduwp
eaxwvxvvwbrovypz
nmluqvobbbmdiwwb
lnkminvfjjzqbmio
mjiiqzycqdhfietz
towlrzriicyraevq
obiloewdvbrsfwjo
lmeooaajlthsfltw
ichygipzpykkesrw
gfysloxmqdsfskvt
saqzntehjldvwtsx
pqddoemaufpfcaew
mjrxvbvwcreaybwe
ngfbrwfqnxqosoai
nesyewxreiqvhald
kqhqdlquywotcyfy
liliptyoqujensfi
nsahsaxvaepzneqq
zaickulfjajhctye
gxjzahtgbgbabtht
koxbuopaqhlsyhrp
jhzejdjidqqtjnwe
dekrkdvprfqpcqki
linwlombdqtdeyop
dvckqqbnigdcmwmx
yaxygbjpzkvnnebv
rlzkdkgaagmcpxah
cfzuyxivtknirqvt
obivkajhsjnrxxhn
lmjhayymgpseuynn
bbjyewkwadaipyju
lmzyhwomfypoftuu
gtzhqlgltvatxack
jfflcfaqqkrrltgq
txoummmnzfrlrmcg
ohemsbfuqqpucups
imsfvowcbieotlok
tcnsnccdszxfcyde
qkcdtkwuaquajazz
arcfnhmdjezdbqku
srnocgyqrlcvlhkb
mppbzvfmcdirbyfw
xiuarktilpldwgwd
ypufwmhrvzqmexpc
itpdnsfkwgrdujmj
cmpxnodtsswkyxkr
wayyxtjklfrmvbfp
mfaxphcnjczhbbwy
sjxhgwdnqcofbdra
pnxmujuylqccjvjm
ivamtjbvairwjqwl
deijtmzgpfxrclss
bzkqcaqagsynlaer
tycefobvxcvwaulz
ctbhnywezxkdsswf
urrxxebxrthtjvib
fpfelcigwqwdjucv
ngfcyyqpqulwcphb
rltkzsiipkpzlgpw
qfdsymzwhqqdkykc
balrhhxipoqzmihj
rnwalxgigswxomga
ghqnxeogckshphgr
lyyaentdizaumnla
exriodwfzosbeoib
speswfggibijfejk
yxmxgfhvmshqszrq
hcqhngvahzgawjga
qmhlsrfpesmeksur
eviafjejygakodla
kvcfeiqhynqadbzv
fusvyhowslfzqttg
girqmvwmcvntrwau
yuavizroykfkdekz
jmcwohvmzvowrhxf
kzimlcpavapynfue
wjudcdtrewfabppq
yqpteuxqgbmqfgxh
xdgiszbuhdognniu
jsguxfwhpftlcjoh
whakkvspssgjzxre
ggvnvjurlyhhijgm
krvbhjybnpemeptr
pqedgfojyjybfbzr
jzhcrsgmnkwwtpdo
yyscxoxwofslncmp
gzjhnxytmyntzths
iteigbnqbtpvqumi
zjevfzusnjukqpfw
xippcyhkfuounxqk
mcnhrcfonfdgpkyh
pinkcyuhjkexbmzj
lotxrswlxbxlxufs
fmqajrtoabpckbnu
wfkwsgmcffdgaqxg
qfrsiwnohoyfbidr
czfqbsbmiuyusaqs
ieknnjeecucghpoo
cevdgqnugupvmsge
gjkajcyjnxdrtuvr
udzhrargnujxiclq
zqqrhhmjwermjssg
ggdivtmgoqajydzz
wnpfsgtxowkjiivl
afbhqawjbotxnqpd
xjpkifkhfjeqifdn
oyfggzsstfhvticp
kercaetahymeawxy
khphblhcgmbupmzt
iggoqtqpvaebtiol
ofknifysuasshoya
qxuewroccsbogrbv
apsbnbkiopopytgu
zyahfroovfjlythh
bxhjwfgeuxlviydq
uvbhdtvaypasaswa
qamcjzrmesqgqdiz
hjnjyzrxntiycyel
wkcrwqwniczwdxgq
hibxlvkqakusswkx
mzjyuenepwdgrkty
tvywsoqslfsulses
jqwcwuuisrclircv
xanwaoebfrzhurct
ykriratovsvxxasf
qyebvtqqxbjuuwuo
telrvlwvriylnder
acksrrptgnhkeiaa
yemwfjhiqlzsvdxf
banrornfkcymmkcc
ytbhxvaeiigjpcgm
crepyazgxquposkn
xlqwdrytzwnxzwzv
xtrbfbwopxscftps
kwbytzukgseeyjla
qtfdvavvjogybxjg
ytbmvmrcxwfkgvzw
nbscbdskdeocnfzr
sqquwjbdxsxhcseg
ewqxhigqcgszfsuw
cvkyfcyfmubzwsee
dcoawetekigxgygd
ohgqnqhfimyuqhvi
otisopzzpvnhctte
bauieohjejamzien
ewnnopzkujbvhwce
aeyqlskpaehagdiv
pncudvivwnnqspxy
ytugesilgveokxcg
zoidxeelqdjesxpr
ducjccsuaygfchzj
smhgllqqqcjfubfc
nlbyyywergronmir
prdawpbjhrzsbsvj
nmgzhnjhlpcplmui
eflaogtjghdjmxxz
qolvpngucbkprrdc
ixywxcienveltgho
mwnpqtocagenkxut
iskrfbwxonkguywx
ouhtbvcaczqzmpua
srewprgddfgmdbao
dyufrltacelchlvu
czmzcbrkecixuwzz
dtbeojcztzauofuk
prrgoehpqhngfgmw
baolzvfrrevxsyke
zqadgxshwiarkzwh
vsackherluvurqqj
surbpxdulvcvgjbd
wqxytarcxzgxhvtx
vbcubqvejcfsgrac
zqnjfeapshjowzja
hekvbhtainkvbynx
knnugxoktxpvoxnh
knoaalcefpgtvlwm
qoakaunowmsuvkus
ypkvlzcduzlezqcb
ujhcagawtyepyogh
wsilcrxncnffaxjf
gbbycjuscquaycrk
aduojapeaqwivnly
ceafyxrakviagcjy
nntajnghicgnrlst
vdodpeherjmmvbje
wyyhrnegblwvdobn
xlfurpghkpbzhhif
xyppnjiljvirmqjo
kglzqahipnddanpi
omjateouxikwxowr
ocifnoopfglmndcx
emudcukfbadyijev
ooktviixetfddfmh
wtvrhloyjewdeycg
cgjncqykgutfjhvb
nkwvpswppeffmwad
hqbcmfhzkxmnrivg
mdskbvzguxvieilr
anjcvqpavhdloaqh
erksespdevjylenq
fadxwbmisazyegup
iyuiffjmcaahowhj
ygkdezmynmltodbv
fytneukxqkjattvh
woerxfadbfrvdcnz
iwsljvkyfastccoa
movylhjranlorofe
drdmicdaiwukemep
knfgtsmuhfcvvshg
ibstpbevqmdlhajn
tstwsswswrxlzrqs
estyydmzothggudf
jezogwvymvikszwa
izmqcwdyggibliet
nzpxbegurwnwrnca
kzkojelnvkwfublh
xqcssgozuxfqtiwi
tcdoigumjrgvczfv
ikcjyubjmylkwlwq
kqfivwystpqzvhan
bzukgvyoqewniivj
iduapzclhhyfladn
fbpyzxdfmkrtfaeg
yzsmlbnftftgwadz
"""

  let day06input = """turn off 660,55 through 986,197
turn off 341,304 through 638,850
turn off 199,133 through 461,193
toggle 322,558 through 977,958
toggle 537,781 through 687,941
turn on 226,196 through 599,390
turn on 240,129 through 703,297
turn on 317,329 through 451,798
turn on 957,736 through 977,890
turn on 263,530 through 559,664
turn on 158,270 through 243,802
toggle 223,39 through 454,511
toggle 544,218 through 979,872
turn on 313,306 through 363,621
toggle 173,401 through 496,407
toggle 333,60 through 748,159
turn off 87,577 through 484,608
turn on 809,648 through 826,999
toggle 352,432 through 628,550
turn off 197,408 through 579,569
turn off 1,629 through 802,633
turn off 61,44 through 567,111
toggle 880,25 through 903,973
turn on 347,123 through 864,746
toggle 728,877 through 996,975
turn on 121,895 through 349,906
turn on 888,547 through 931,628
toggle 398,782 through 834,882
turn on 966,850 through 989,953
turn off 891,543 through 914,991
toggle 908,77 through 916,117
turn on 576,900 through 943,934
turn off 580,170 through 963,206
turn on 184,638 through 192,944
toggle 940,147 through 978,730
turn off 854,56 through 965,591
toggle 717,172 through 947,995
toggle 426,987 through 705,998
turn on 987,157 through 992,278
toggle 995,774 through 997,784
turn off 796,96 through 845,182
turn off 451,87 through 711,655
turn off 380,93 through 968,676
turn on 263,468 through 343,534
turn on 917,936 through 928,959
toggle 478,7 through 573,148
turn off 428,339 through 603,624
turn off 400,880 through 914,953
toggle 679,428 through 752,779
turn off 697,981 through 709,986
toggle 482,566 through 505,725
turn off 956,368 through 993,516
toggle 735,823 through 783,883
turn off 48,487 through 892,496
turn off 116,680 through 564,819
turn on 633,865 through 729,930
turn off 314,618 through 571,922
toggle 138,166 through 936,266
turn on 444,732 through 664,960
turn off 109,337 through 972,497
turn off 51,432 through 77,996
turn off 259,297 through 366,744
toggle 801,130 through 917,544
toggle 767,982 through 847,996
turn on 216,507 through 863,885
turn off 61,441 through 465,731
turn on 849,970 through 944,987
toggle 845,76 through 852,951
toggle 732,615 through 851,936
toggle 251,128 through 454,778
turn on 324,429 through 352,539
toggle 52,450 through 932,863
turn off 449,379 through 789,490
turn on 317,319 through 936,449
toggle 887,670 through 957,838
toggle 671,613 through 856,664
turn off 186,648 through 985,991
turn off 471,689 through 731,717
toggle 91,331 through 750,758
toggle 201,73 through 956,524
toggle 82,614 through 520,686
toggle 84,287 through 467,734
turn off 132,367 through 208,838
toggle 558,684 through 663,920
turn on 237,952 through 265,997
turn on 694,713 through 714,754
turn on 632,523 through 862,827
turn on 918,780 through 948,916
turn on 349,586 through 663,976
toggle 231,29 through 257,589
toggle 886,428 through 902,993
turn on 106,353 through 236,374
turn on 734,577 through 759,684
turn off 347,843 through 696,912
turn on 286,699 through 964,883
turn on 605,875 through 960,987
turn off 328,286 through 869,461
turn off 472,569 through 980,848
toggle 673,573 through 702,884
turn off 398,284 through 738,332
turn on 158,50 through 284,411
turn off 390,284 through 585,663
turn on 156,579 through 646,581
turn on 875,493 through 989,980
toggle 486,391 through 924,539
turn on 236,722 through 272,964
toggle 228,282 through 470,581
toggle 584,389 through 750,761
turn off 899,516 through 900,925
turn on 105,229 through 822,846
turn off 253,77 through 371,877
turn on 826,987 through 906,992
turn off 13,152 through 615,931
turn on 835,320 through 942,399
turn on 463,504 through 536,720
toggle 746,942 through 786,998
turn off 867,333 through 965,403
turn on 591,477 through 743,692
turn off 403,437 through 508,908
turn on 26,723 through 368,814
turn on 409,485 through 799,809
turn on 115,630 through 704,705
turn off 228,183 through 317,220
toggle 300,649 through 382,842
turn off 495,365 through 745,562
turn on 698,346 through 744,873
turn on 822,932 through 951,934
toggle 805,30 through 925,421
toggle 441,152 through 653,274
toggle 160,81 through 257,587
turn off 350,781 through 532,917
toggle 40,583 through 348,636
turn on 280,306 through 483,395
toggle 392,936 through 880,955
toggle 496,591 through 851,934
turn off 780,887 through 946,994
turn off 205,735 through 281,863
toggle 100,876 through 937,915
turn on 392,393 through 702,878
turn on 956,374 through 976,636
toggle 478,262 through 894,775
turn off 279,65 through 451,677
turn on 397,541 through 809,847
turn on 444,291 through 451,586
toggle 721,408 through 861,598
turn on 275,365 through 609,382
turn on 736,24 through 839,72
turn off 86,492 through 582,712
turn on 676,676 through 709,703
turn off 105,710 through 374,817
toggle 328,748 through 845,757
toggle 335,79 through 394,326
toggle 193,157 through 633,885
turn on 227,48 through 769,743
toggle 148,333 through 614,568
toggle 22,30 through 436,263
toggle 547,447 through 688,969
toggle 576,621 through 987,740
turn on 711,334 through 799,515
turn on 541,448 through 654,951
toggle 792,199 through 798,990
turn on 89,956 through 609,960
toggle 724,433 through 929,630
toggle 144,895 through 201,916
toggle 226,730 through 632,871
turn off 760,819 through 828,974
toggle 887,180 through 940,310
toggle 222,327 through 805,590
turn off 630,824 through 885,963
turn on 940,740 through 954,946
turn on 193,373 through 779,515
toggle 304,955 through 469,975
turn off 405,480 through 546,960
turn on 662,123 through 690,669
turn off 615,238 through 750,714
turn on 423,220 through 930,353
turn on 329,769 through 358,970
toggle 590,151 through 704,722
turn off 884,539 through 894,671
toggle 449,241 through 984,549
toggle 449,260 through 496,464
turn off 306,448 through 602,924
turn on 286,805 through 555,901
toggle 722,177 through 922,298
toggle 491,554 through 723,753
turn on 80,849 through 174,996
turn off 296,561 through 530,856
toggle 653,10 through 972,284
toggle 529,236 through 672,614
toggle 791,598 through 989,695
turn on 19,45 through 575,757
toggle 111,55 through 880,871
turn off 197,897 through 943,982
turn on 912,336 through 977,605
toggle 101,221 through 537,450
turn on 101,104 through 969,447
toggle 71,527 through 587,717
toggle 336,445 through 593,889
toggle 214,179 through 575,699
turn on 86,313 through 96,674
toggle 566,427 through 906,888
turn off 641,597 through 850,845
turn on 606,524 through 883,704
turn on 835,775 through 867,887
toggle 547,301 through 897,515
toggle 289,930 through 413,979
turn on 361,122 through 457,226
turn on 162,187 through 374,746
turn on 348,461 through 454,675
turn off 966,532 through 985,537
turn on 172,354 through 630,606
turn off 501,880 through 680,993
turn off 8,70 through 566,592
toggle 433,73 through 690,651
toggle 840,798 through 902,971
toggle 822,204 through 893,760
turn off 453,496 through 649,795
turn off 969,549 through 990,942
turn off 789,28 through 930,267
toggle 880,98 through 932,434
toggle 568,674 through 669,753
turn on 686,228 through 903,271
turn on 263,995 through 478,999
toggle 534,675 through 687,955
turn off 342,434 through 592,986
toggle 404,768 through 677,867
toggle 126,723 through 978,987
toggle 749,675 through 978,959
turn off 445,330 through 446,885
turn off 463,205 through 924,815
turn off 417,430 through 915,472
turn on 544,990 through 912,999
turn off 201,255 through 834,789
turn off 261,142 through 537,862
turn off 562,934 through 832,984
turn off 459,978 through 691,980
turn off 73,911 through 971,972
turn on 560,448 through 723,810
turn on 204,630 through 217,854
turn off 91,259 through 611,607
turn on 877,32 through 978,815
turn off 950,438 through 974,746
toggle 426,30 through 609,917
toggle 696,37 through 859,201
toggle 242,417 through 682,572
turn off 388,401 through 979,528
turn off 79,345 through 848,685
turn off 98,91 through 800,434
toggle 650,700 through 972,843
turn off 530,450 through 538,926
turn on 428,559 through 962,909
turn on 78,138 through 92,940
toggle 194,117 through 867,157
toggle 785,355 through 860,617
turn off 379,441 through 935,708
turn off 605,133 through 644,911
toggle 10,963 through 484,975
turn off 359,988 through 525,991
turn off 509,138 through 787,411
toggle 556,467 through 562,773
turn on 119,486 through 246,900
turn on 445,561 through 794,673
turn off 598,681 through 978,921
turn off 974,230 through 995,641
turn off 760,75 through 800,275
toggle 441,215 through 528,680
turn off 701,636 through 928,877
turn on 165,753 through 202,780
toggle 501,412 through 998,516
toggle 161,105 through 657,395
turn on 113,340 through 472,972
toggle 384,994 through 663,999
turn on 969,994 through 983,997
turn on 519,600 through 750,615
turn off 363,899 through 948,935
turn on 271,845 through 454,882
turn off 376,528 through 779,640
toggle 767,98 through 854,853
toggle 107,322 through 378,688
turn off 235,899 through 818,932
turn on 445,611 through 532,705
toggle 629,387 through 814,577
toggle 112,414 through 387,421
toggle 319,184 through 382,203
turn on 627,796 through 973,940
toggle 602,45 through 763,151
turn off 441,375 through 974,545
toggle 871,952 through 989,998
turn on 717,272 through 850,817
toggle 475,711 through 921,882
toggle 66,191 through 757,481
turn off 50,197 through 733,656
toggle 83,575 through 915,728
turn on 777,812 through 837,912
turn on 20,984 through 571,994
turn off 446,432 through 458,648
turn on 715,871 through 722,890
toggle 424,675 through 740,862
toggle 580,592 through 671,900
toggle 296,687 through 906,775"""

  let day07input = """
bn RSHIFT 2 -> bo
lf RSHIFT 1 -> ly
fo RSHIFT 3 -> fq
cj OR cp -> cq
fo OR fz -> ga
t OR s -> u
lx -> a
NOT ax -> ay
he RSHIFT 2 -> hf
lf OR lq -> lr
lr AND lt -> lu
dy OR ej -> ek
1 AND cx -> cy
hb LSHIFT 1 -> hv
1 AND bh -> bi
ih AND ij -> ik
c LSHIFT 1 -> t
ea AND eb -> ed
km OR kn -> ko
NOT bw -> bx
ci OR ct -> cu
NOT p -> q
lw OR lv -> lx
NOT lo -> lp
fp OR fv -> fw
o AND q -> r
dh AND dj -> dk
ap LSHIFT 1 -> bj
bk LSHIFT 1 -> ce
NOT ii -> ij
gh OR gi -> gj
kk RSHIFT 1 -> ld
lc LSHIFT 1 -> lw
lb OR la -> lc
1 AND am -> an
gn AND gp -> gq
lf RSHIFT 3 -> lh
e OR f -> g
lg AND lm -> lo
ci RSHIFT 1 -> db
cf LSHIFT 1 -> cz
bn RSHIFT 1 -> cg
et AND fe -> fg
is OR it -> iu
kw AND ky -> kz
ck AND cl -> cn
bj OR bi -> bk
gj RSHIFT 1 -> hc
iu AND jf -> jh
NOT bs -> bt
kk OR kv -> kw
ks AND ku -> kv
hz OR ik -> il
b RSHIFT 1 -> v
iu RSHIFT 1 -> jn
fo RSHIFT 5 -> fr
be AND bg -> bh
ga AND gc -> gd
hf OR hl -> hm
ld OR le -> lf
as RSHIFT 5 -> av
fm OR fn -> fo
hm AND ho -> hp
lg OR lm -> ln
NOT kx -> ky
kk RSHIFT 3 -> km
ek AND em -> en
NOT ft -> fu
NOT jh -> ji
jn OR jo -> jp
gj AND gu -> gw
d AND j -> l
et RSHIFT 1 -> fm
jq OR jw -> jx
ep OR eo -> eq
lv LSHIFT 15 -> lz
NOT ey -> ez
jp RSHIFT 2 -> jq
eg AND ei -> ej
NOT dm -> dn
jp AND ka -> kc
as AND bd -> bf
fk OR fj -> fl
dw OR dx -> dy
lj AND ll -> lm
ec AND ee -> ef
fq AND fr -> ft
NOT kp -> kq
ki OR kj -> kk
cz OR cy -> da
as RSHIFT 3 -> au
an LSHIFT 15 -> ar
fj LSHIFT 15 -> fn
1 AND fi -> fj
he RSHIFT 1 -> hx
lf RSHIFT 2 -> lg
kf LSHIFT 15 -> kj
dz AND ef -> eh
ib OR ic -> id
lf RSHIFT 5 -> li
bp OR bq -> br
NOT gs -> gt
fo RSHIFT 1 -> gh
bz AND cb -> cc
ea OR eb -> ec
lf AND lq -> ls
NOT l -> m
hz RSHIFT 3 -> ib
NOT di -> dj
NOT lk -> ll
jp RSHIFT 3 -> jr
jp RSHIFT 5 -> js
NOT bf -> bg
s LSHIFT 15 -> w
eq LSHIFT 1 -> fk
jl OR jk -> jm
hz AND ik -> im
dz OR ef -> eg
1 AND gy -> gz
la LSHIFT 15 -> le
br AND bt -> bu
NOT cn -> co
v OR w -> x
d OR j -> k
1 AND gd -> ge
ia OR ig -> ih
NOT go -> gp
NOT ed -> ee
jq AND jw -> jy
et OR fe -> ff
aw AND ay -> az
ff AND fh -> fi
ir LSHIFT 1 -> jl
gg LSHIFT 1 -> ha
x RSHIFT 2 -> y
db OR dc -> dd
bl OR bm -> bn
ib AND ic -> ie
x RSHIFT 3 -> z
lh AND li -> lk
ce OR cd -> cf
NOT bb -> bc
hi AND hk -> hl
NOT gb -> gc
1 AND r -> s
fw AND fy -> fz
fb AND fd -> fe
1 AND en -> eo
z OR aa -> ab
bi LSHIFT 15 -> bm
hg OR hh -> hi
kh LSHIFT 1 -> lb
cg OR ch -> ci
1 AND kz -> la
gf OR ge -> gg
gj RSHIFT 2 -> gk
dd RSHIFT 2 -> de
NOT ls -> lt
lh OR li -> lj
jr OR js -> jt
au AND av -> ax
0 -> c
he AND hp -> hr
id AND if -> ig
et RSHIFT 5 -> ew
bp AND bq -> bs
e AND f -> h
ly OR lz -> ma
1 AND lu -> lv
NOT jd -> je
ha OR gz -> hb
dy RSHIFT 1 -> er
iu RSHIFT 2 -> iv
NOT hr -> hs
as RSHIFT 1 -> bl
kk RSHIFT 2 -> kl
b AND n -> p
ln AND lp -> lq
cj AND cp -> cr
dl AND dn -> do
ci RSHIFT 2 -> cj
as OR bd -> be
ge LSHIFT 15 -> gi
hz RSHIFT 5 -> ic
dv LSHIFT 1 -> ep
kl OR kr -> ks
gj OR gu -> gv
he RSHIFT 5 -> hh
NOT fg -> fh
hg AND hh -> hj
b OR n -> o
jk LSHIFT 15 -> jo
gz LSHIFT 15 -> hd
cy LSHIFT 15 -> dc
kk RSHIFT 5 -> kn
ci RSHIFT 3 -> ck
at OR az -> ba
iu RSHIFT 3 -> iw
ko AND kq -> kr
NOT eh -> ei
aq OR ar -> as
iy AND ja -> jb
dd RSHIFT 3 -> df
bn RSHIFT 3 -> bp
1 AND cc -> cd
at AND az -> bb
x OR ai -> aj
kk AND kv -> kx
ao OR an -> ap
dy RSHIFT 3 -> ea
x RSHIFT 1 -> aq
eu AND fa -> fc
kl AND kr -> kt
ia AND ig -> ii
df AND dg -> di
NOT fx -> fy
k AND m -> n
bn RSHIFT 5 -> bq
km AND kn -> kp
dt LSHIFT 15 -> dx
hz RSHIFT 2 -> ia
aj AND al -> am
cd LSHIFT 15 -> ch
hc OR hd -> he
he RSHIFT 3 -> hg
bn OR by -> bz
NOT kt -> ku
z AND aa -> ac
NOT ak -> al
cu AND cw -> cx
NOT ie -> if
dy RSHIFT 2 -> dz
ip LSHIFT 15 -> it
de OR dk -> dl
au OR av -> aw
jg AND ji -> jj
ci AND ct -> cv
dy RSHIFT 5 -> eb
hx OR hy -> hz
eu OR fa -> fb
gj RSHIFT 3 -> gl
fo AND fz -> gb
1 AND jj -> jk
jp OR ka -> kb
de AND dk -> dm
ex AND ez -> fa
df OR dg -> dh
iv OR jb -> jc
x RSHIFT 5 -> aa
NOT hj -> hk
NOT im -> in
fl LSHIFT 1 -> gf
hu LSHIFT 15 -> hy
iq OR ip -> ir
iu RSHIFT 5 -> ix
NOT fc -> fd
NOT el -> em
ck OR cl -> cm
et RSHIFT 3 -> ev
hw LSHIFT 1 -> iq
ci RSHIFT 5 -> cl
iv AND jb -> jd
dd RSHIFT 5 -> dg
as RSHIFT 2 -> at
NOT jy -> jz
af AND ah -> ai
1 AND ds -> dt
jx AND jz -> ka
da LSHIFT 1 -> du
fs AND fu -> fv
jp RSHIFT 1 -> ki
iw AND ix -> iz
iw OR ix -> iy
eo LSHIFT 15 -> es
ev AND ew -> ey
ba AND bc -> bd
fp AND fv -> fx
jc AND je -> jf
et RSHIFT 2 -> eu
kg OR kf -> kh
iu OR jf -> jg
er OR es -> et
fo RSHIFT 2 -> fp
NOT ca -> cb
bv AND bx -> by
u LSHIFT 1 -> ao
cm AND co -> cp
y OR ae -> af
bn AND by -> ca
1 AND ke -> kf
jt AND jv -> jw
fq OR fr -> fs
dy AND ej -> el
NOT kc -> kd
ev OR ew -> ex
dd OR do -> dp
NOT cv -> cw
gr AND gt -> gu
dd RSHIFT 1 -> dw
NOT gw -> gx
NOT iz -> ja
1 AND io -> ip
NOT ag -> ah
b RSHIFT 5 -> f
NOT cr -> cs
kb AND kd -> ke
jr AND js -> ju
cq AND cs -> ct
il AND in -> io
NOT ju -> jv
du OR dt -> dv
dd AND do -> dq
b RSHIFT 2 -> d
jm LSHIFT 1 -> kg
NOT dq -> dr
bo OR bu -> bv
gk OR gq -> gr
he OR hp -> hq
NOT h -> i
hf AND hl -> hn
gv AND gx -> gy
x AND ai -> ak
bo AND bu -> bw
hq AND hs -> ht
hz RSHIFT 1 -> is
gj RSHIFT 5 -> gm
g AND i -> j
gk AND gq -> gs
dp AND dr -> ds
b RSHIFT 3 -> e
gl AND gm -> go
gl OR gm -> gn
y AND ae -> ag
hv OR hu -> hw
1674 -> b
ab AND ad -> ae
NOT ac -> ad
1 AND ht -> hu
NOT hn -> ho
"""

  let day08input = """
"azlgxdbljwygyttzkfwuxv"
"v\xfb\"lgs\"kvjfywmut\x9cr"
"merxdhj"
"dwz"
"d\\gkbqo\\fwukyxab\"u"
"k\xd4cfixejvkicryipucwurq\x7eq"
"nvtidemacj\"hppfopvpr"
"kbngyfvvsdismznhar\\p\"\"gpryt\"jaeh"
"khre\"o\x0elqfrbktzn"
"nugkdmqwdq\x50amallrskmrxoyo"
"jcrkptrsasjp\\\"cwigzynjgspxxv\\vyb"
"ramf\"skhcmenhbpujbqwkltmplxygfcy"
"aqjqgbfqaxga\\fkdcahlfi\"pvods"
"pcrtfb"
"\x83qg\"nwgugfmfpzlrvty\"ryoxm"
"fvhvvokdnl\\eap"
"kugdkrat"
"seuxwc"
"vhioftcosshaqtnz"
"gzkxqrdq\\uko\"mrtst"
"znjcomvy\x16hhsenmroswr"
"clowmtra"
"\xc4"
"jpavsevmziklydtqqm"
"egxjqytcttr\\ecfedmmovkyn\"m"
"mjulrvqgmsvmwf"
"o\\prxtlfbatxerhev\xf9hcl\x44rzmvklviv"
"lregjexqaqgwloydxdsc\\o\"dnjfmjcu"
"lnxluajtk\x8desue\\k\x7abhwokfhh"
"wrssfvzzn\"llrysjgiu\"npjtdli"
"\x67lwkks"
"bifw\"ybvmwiyi\"vhol\"vol\xd4"
"aywdqhvtvcpvbewtwuyxrix"
"gc\xd3\"caukdgfdywj"
"uczy\\fk"
"bnlxkjvl\x7docehufkj\\\"qoyhag"
"bidsptalmoicyorbv\\"
"jorscv\"mufcvvfmcv\"ga"
"sofpwfal\\a"
"kcuqtbboaly\"uj\"k"
"n\\c"
"x\"\xcaj\\xwwvpdldz"
"eyukphh"
"wcyjq"
"vjx\"\"hjroj\"l\x4cjwbr"
"xcodsxzfqw\\rowqtuwvjnxupjnrh"
"yc"
"fpvzldgbdtca\"hqwa"
"ymjq\x8ahohvafubra\"hgqoknkuyph"
"kx\\mkaaklvcup"
"belddrzegcsxsyfhzyz"
"fuyswi"
"\\hubzebo\"ha\\qyr\"dv\\"
"mxvlz\"fwuvx\"cyk\""
"ftbh\"ro\\tmcpnpvh\"xx"
"ygi"
"rw\"\"wwn\\fgbjumq\"vgvoh\xd0\"mm"
"\"pat\"\x63kpfc\"\x2ckhfvxk\"uwqzlx"
"o"
"d\"hqtsfp\xceaswe\"\xc0lw"
"zajpvfawqntvoveal\"\"trcdarjua"
"xzapq"
"rkmhm"
"byuq"
"rwwmt\xe8jg\xc2\"omt"
"nfljgdmgefvlh\"x"
"rpjxcexisualz"
"doxcycmgaiptvd"
"rq\\\"mohnjdf\\xv\\hrnosdtmvxot"
"oqvbcenib\"uhy\\npjxg"
"pkvgnm\\ruayuvpbpd"
"kknmzpxqfbcdgng"
"piduhbmaympxdexz"
"vapczawekhoa\\or"
"tlwn\"avc\"bycg\"\"xuxea"
"\xcdvryveteqzxrgopmdmihkcgsuozips"
"kpzziqt"
"sdy\\s\"cjq"
"yujs"
"qte\"q"
"qyvpnkhjcqjv\"cclvv\"pclgtg\xeak\"tno"
"xwx"
"vibuvv"
"qq\""
"wwjduomtbkbdtorhpyalxswisq\"r"
"afuw\\mfjzctcivwesutxbk\"lk"
"e\xcef\\hkiu"
"ftdrgzvygcw\"jwsrcmgxj"
"zrddqfkx\x21dr\"ju\"elybk\"powj\"\"kpryz"
"dttdkfvbodkma\""
"lzygktugpqw"
"qu\x83tes\\u\"tnid\"ryuz"
"\\o\"pe\\vqwlsizjklwrjofg\xe2oau\\rd"
"mikevjzhnwgx\"fozrj\"h\""
"ligxmxznzvtachvvbahnff"
"d\\kq"
"tnbkxpzmcakqhaa"
"g\\yeakebeyv"
"cqkcnd\"sxjxfnawy\x31zax\x6ceha"
"m\x0dtqotffzdnetujtsgjqgwddc"
"masnugb\"etgmxul\x3bqd\\tmtddnvcy"
"floediikodfgre\x23wyoxlswxflwecdjpt"
"zu"
"r"
"\"ashzdbd\"pdvba\xeeumkr\\amnj"
"ckslmuwbtfouwpfwtuiqmeozgspwnhx"
"t\\qjsjek\xf9gjcxsyco\"r"
"hoed\x1b\\tcmaqch\"epdy"
"mgjiojwzc\\ypqcn\xb1njmp\"aeeblxt"
"\xdf\"h\x5enfracj"
"\x6fpbpocrb"
"jbmhrswyyq\\"
"wtyqtenfwatji\"ls\\"
"voy"
"awj"
"rtbj\"j"
"hynl"
"orqqeuaat\\xu\\havsgr\xc5qdk"
"g\"npyzjfq\"rjefwsk"
"rk\\kkcirjbixr\\zelndx\"bsnqvqj\""
"tecoz"
"dn\"uswngbdk\""
"qb\\"
"wpyis\\ebq"
"ppwue\\airoxzjjdqbvyurhaabetv"
"fxlvt"
"ql\"oqsmsvpxcg\"k"
"vqlhuec\\adw"
"qzmi\xffberakqqkk"
"tisjqff\"wf"
"yhnpudoaybwucvppj"
"xhfuf\\ehsrhsnfxcwtibd\"ubfpz"
"ihgjquzhf\""
"ff\x66dsupesrnusrtqnywoqcn\\"
"z\x77zpubbjmd"
"\"vhzlbwq\"xeimjt\\xe\x85umho\"m\"\"bmy"
"mmuvkioocmzjjysi\"mkfbec\""
"rpgghowbduw\x2fayslubajinoik\xd0hcfy"
"xrkyjqul\xdexlojgdphczp\"jfk"
"mg\x07cnr\x8b\x67xdgszmgiktpjhawho"
"kdgufhaoab"
"rlhela\"nldr"
"wzye\x87u"
"yif\x75bjhnitgoarmfgqwpmopu"
"pvlbyez\"wyy\x3dpgr"
"ezdm\"ovkruthkvdwtqwr\"ibdoawzgu"
"qubp"
"b\\kcpegcn\\zgdemgorjnk"
"gjsva\\kzaor\"\"gtpd"
"\"kt"
"rlymwlcodix"
"qqtmswowxca\"jvv"
"jni\xebwhozb"
"zhino\"kzjtmgxpi\"zzexijg"
"tyrbat\\mejgzplufxixkyg"
"lhmopxiao\x09\"p\xebl"
"xefioorxvate"
"nmcgd\x46xfujt\"w"
"\xe3wnwpat\"gtimrb"
"wpq\"xkjuw\xebbohgcagppb"
"fmvpwaca"
"mlsw"
"fdan\\\x9e"
"\"f\"fmdlzc"
"nyuj\\jnnfzdnrqmhvjrahlvzl"
"zn\"f\xcfsshcdaukkimfwk"
"uayugezzo\\\"e\"blnrgjaupqhik"
"efd\"apkndelkuvfvwyyatyttkehc"
"ufxq\\\"m\"bwkh\x93kapbqrvxxzbzp\\"
"fgypsbgjak\x79qblbeidavqtddfacq\\i\"h"
"kcfgpiysdxlgejjvgndb\\dovfpqodw"
"\"onpqnssmighipuqgwx\"nrokzgvg"
"vhjrrhfrba\"jebdanzsrdusut\\wbs"
"o\xdakymbaxakys"
"uwxhhzz\\mtmhghjn\\\\tnhzbejj"
"yd\\"
"bpgztp\\lzwpdqju\"it\x35qjhihjv"
"\\my\\b\"klnnto\\\xb3mbtsh"
"ezyvknv\"l\x2bdhhfjcvwzhjgmhwbqd\"\\"
"ftkz\"amoncbsohtaumhl\"wsodemopodq"
"ifv"
"dmzfxvzq"
"sped\"bvmf\"mmevl\"zydannpfny"
"fjxcjwlv\"pnqyrzatsjwsqfidb"
"muc\xfdqouwwnmuixru\\zlhjintplvtee"
"mraqgvmj"
"njopq\"ftcsryo"
"enoh\"n"
"t\"ntjhjc\"nzqh\xf7dcohhlsja\x7dtr"
"flbqcmcoun"
"dxkiysrn\\dyuqoaig"
"nehkzi\"h\"syktzfufotng\xdafqo"
"dzkjg\\hqjk\\\"zfegssjhn"
"sadlsjv"
"vmfnrdb\""
"ac\\bdp\"n"
"qt\x89h"
"lsndeugwvijwde\\vjapbm\\k\\nljuva"
"twpmltdzyynqt\\z\\tnund\x64hm"
"hpcyata\"ocylbkzdnhujh"
"hskzq\"knntuhscex\"q\\y\\vqj\x3an"
"eekwyufvji\\mqgeroekxeyrmymq"
"hl\"durthetvri\xebw\\jxu\"rcmiuy"
"\"fxdnmvnftxwesmvvq\"sjnf\xaabpg\"iary"
"\"\"nksqso"
"ruq\xbezugge\"d\"hwvoxmy\"iawikddxn\"x"
"rxxnlfay"
"stcu\"mv\xabcqts\\fasff"
"yrnvwfkfuzuoysfdzl\x02bk"
"qbdsmlwdbfknivtwijbwtatqfe"
"\"erqh\\csjph"
"ikfv"
"\xd2cuhowmtsxepzsivsvnvsb"
"vj"
"d"
"\\g"
"porvg\x62qghorthnc\"\\"
"tiks\\kr\"\x0fuejvuxzswnwdjscrk"
"xmgfel\"atma\\zaxmlgfjx\"ajmqf"
"oz\\rnxwljc\\\"umhymtwh"
"wlsxxhm\x7fqx\\gjoyrvccfiner\\qloluqv"
"k\\ieq"
"xidjj\"ksnlgnwxlddf\\s\\kuuleb"
"wjpnzgprzv\\maub\x0cj"
"r"
"y"
"\"yecqiei\"ire\\jdhlnnlde\xc5u"
"drvdiycqib"
"egnrbefezcrhgldrtb"
"plqodxv\\zm\"uodwjdocri\x55ucaezutm"
"f\"wexcw\x02ekewx\"alyzn"
"pqajwuk\\\\oatkfqdyspnrupo"
"rkczj\"fzntabpnygrhamk\\km\x68xfkmr"
"wejam\xbac\x37kns"
"qqmlwjk\"gh"
"fdcjsxlgx"
"\\cxvxy\"kb\"\"unubvrsq\\y\\awfhbmarj\\"
"geunceaqr"
"tpkg\"svvngk\\sizlsyaqwf"
"\"pa\\x\x18od\\emgje\\"
"ffiizogjjptubzqfuh\"cctieqcdh"
"yikhiyyrpgglpos"
"h\\"
"jotqojodcv"
"ervsz\x87ade\"fevq\\tcqowt"
"\\y\"fgrxtppkcseeg\\onxjarx\\hyhfn\x5fi"
"kxndlabn\\wwumctuzdcfiitrbnn"
"eoosynwhwm"
"\"c\x04"
"ny\xf6vuwlec"
"ubgxxcvnltzaucrzg\\xcez"
"pnocjvo\\yt"
"fcabrtqog\"a\"zj"
"o\\bha\\mzxmrfltnflv\xea"
"tbfvzwhexsdxjmxejwqqngzixcx"
"wdptrakok\"rgymturdmwfiwu"
"reffmj"
"lqm"
"\\oc"
"p\""
"ygkdnhcuehlx"
"vsqmv\"bqay\"olimtkewedzm"
"isos\x6azbnkojhxoopzetbj\xe1yd"
"yo\\pgayjcyhshztnbdv"
"fg\"h"
"vcmcojolfcf\\\\oxveua"
"w\"vyszhbrr\"jpeddpnrjlca\x69bdbopd\\z"
"jikeqv"
"\"dkjdfrtj"
"is"
"hgzx"
"z\""
"woubquq\\ag\""
"xvclriqa\xe6ltt"
"tfxinifmd"
"mvywzf\"jz"
"vlle"
"c\"rf\"wynhye\x25vccvb\""
"zvuxm"
"\xf2\"jdstiwqer\"h"
"kyogyogcknbzv\x9f\\\\e"
"kspodj\"edpeqgypc"
"oh\\x\\h"
"julb"
"bmcfkidxyilgoy\\xmu\"ig\\qg"
"veqww\"ea"
"fkdbemtgtkpqisrwlxutllxc\"mbelhs"
"e"
"ecn\x50ooprbstnq"
"\"\xe8\"ec\xeah\"qo\\g\"iuqxy\"e\"y\xe7xk\xc6d"
"lwj\"aftrcqj"
"jduij\x97zk\"rftjrixzgscxxllpqx\"bwwb"
"fqcditz"
"f\x19azclj\"rsvaokgvty\"aeq"
"erse\x9etmzhlmhy\x67yftoti"
"lsdw\xb3dmiy\\od"
"x\x6fxbljsjdgd\xaau"
"hjg\\w\"\x78uoqbsdikbjxpip\"w\"jnhzec"
"gk"
"\\zrs\\syur"
"""

  let day09input = """
Tristram to AlphaCentauri = 34
Tristram to Snowdin = 100
Tristram to Tambi = 63
Tristram to Faerun = 108
Tristram to Norrath = 111
Tristram to Straylight = 89
Tristram to Arbre = 132
AlphaCentauri to Snowdin = 4
AlphaCentauri to Tambi = 79
AlphaCentauri to Faerun = 44
AlphaCentauri to Norrath = 147
AlphaCentauri to Straylight = 133
AlphaCentauri to Arbre = 74
Snowdin to Tambi = 105
Snowdin to Faerun = 95
Snowdin to Norrath = 48
Snowdin to Straylight = 88
Snowdin to Arbre = 7
Tambi to Faerun = 68
Tambi to Norrath = 134
Tambi to Straylight = 107
Tambi to Arbre = 40
Faerun to Norrath = 11
Faerun to Straylight = 66
Faerun to Arbre = 144
Norrath to Straylight = 115
Norrath to Arbre = 135
Straylight to Arbre = 127
"""

  let day10input = "3113322113"

  let lastData = ""
