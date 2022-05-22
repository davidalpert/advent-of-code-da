namespace AdventOfCode

module Input =
    let splitToTrimmedLines (input: string) =
        input.Trim().Split("\n")
        |> Seq.map (fun s -> s.Trim())

    let splitToTrimmedLinesVerbose (input: string) =
        printfn "input: %A" input
        printfn "input.trimmed: %A" (input.Trim())
        printfn "input.trimmed.split: %A" (input.Trim().Split("\n"))

        input.Trim().Split("\n")
        |> Seq.map (fun s -> s.Trim())

    let integersFromInput (input: string) =
        splitToTrimmedLines input
        |> Seq.map (fun s -> s |> int)

    //#region day01

    let day01data =
        "()()(()()()(()()((()((()))((()((((()()((((()))()((((())(((((((()(((((((((()(((())(()()(()((()()(()(())(()((((()((()()()((((())((((((()(()(((()())(()((((()))())(())(()(()()))))))))((((((((((((()())()())())(())))(((()()()((((()(((()(()(()()(()(()()(()(((((((())(())(())())))((()())()((((()()((()))(((()()()())))(())))((((())(((()())(())(()))(()((((()())))())((()(())(((()((((()((()(())())))((()))()()(()(()))))((((((((()())((((()()((((()(()())(((((()(()())()))())(((()))()(()(()(()((((()(())(()))(((((()()(()()()(()(((())())(((()()(()()))(((()()(((())())(()(())())()()(())()()()((()(((()(())((()()((())()))((()()))((()()())((((()(()()(()(((()))()(()))))((()(((()()()))(()(((())()(()((()())(()(()()(()())(())()(((()(()())()((((()((()))))())()))((()()()()(())()())()()()((((()))))(()(((()()(((((((())()))()((((()((())()(()())(())()))(()(()())(((((((())))(((()))())))))()))())((())(()()((())()())()))))()((()()())(())((())((((()())())()()()(((()))())))()()))())(()()()(()((((((()()))())()))()(((()(((())((((()()()(()))())()()))))())()))())((())()())(((((())())((())())))(((())(((())(((((()(((((())(()(()())())(()(())(()))(()((((()))())()))))())))((()(()))))())))(((((())()))())()))))()))))(((()))()))))((()))((()((()(()(())()())))(()()()(())()))()((((())))))))(())(()((()()))(()))(()))(()((()))))))()()((((()()))()())()))))))()()()))(()((())(()))((()()()())()(((()((((())())))()((((()(()))))))())))()()())()))(()))))(()())()))))))((())))))))())()))()((())())))(()((()))()))(())))))(()))()())()()))((()(()))()()()()))))())()()))())(())()()))()))((()))))()()(()())))))()()()))((((()))()))))(()(())))(()())))((())())(()))()))))()())))()())()())))))))))()()))))())))((())((()))))())))(((()())))))))(()))()()))(()))()))))()())))))())((((()())))))))())))()()))))))))()))()))))()))))))(())))))))))())))))))))))))))())())((())))))))))()))((())))()))))))))())()(()))))))())))))()()()())()(()()()(()())(()))()()()(()())))())())))()))))())))))))()()()()())(())())()())()))))(()()()()()))))()))())())))((()())()())))()))()))))(()())))()))))))))(((()))()()))))))))))))))))))))(()))(()((()))())))())(()))(()(()(())))))()(()))()))()()))))))))))))()((()())(())())()(())))))())()())((()()))))(()()))))())()(())()))))))))))))))))))))()))(()(()())))))))()()((()))()))))))((())))()))))))))((()))())()()))())()()))((()))())))))))))))(()())()))(())((()(()()))(()())(())))()())(()(())()()))))()))()(()))))))(()))))))))))(()))())))))))))())))))())))(())))))()))))(())())))))))))()(()))))()())))())(()))()())))))))))))))())()()))))()))))))())))))()))))(())(()()()()((())()))())(()))((())()))())())(())(()()))))()))(())()()((())(())))(())))()))())))))))))()(((((())())))(())()))))(())))((()))()(((((((()))))()()))(())))))()(()))))(()()))()))())))))))(()())()))))))))())))(()))())()))(())()((())())()())())(()(()))))()))))))((()())(())()()(()())))()()))(())(())(()))())))()))(()))()()))((((()))))()))((()()()))))()))()))())))(()))()))))(())))()))())()(()))()())))())))))))())))())))()()))))))(()))())())))()))()()())())))))))))))))())))()))(()()))))())))())()(())))())))))))))))))))))()()())())))))()()()((()(()))()()(())()())()))()))))()()()))))))((()))))))))()(()(()((((((()()((()())))))))))))()))())))))((())())(()))())))())))))())()()())(())))())))()())())(())))))))()()(())))()))())))())())())()))))))))()))(()()()())())())))(())())))))))()()())()))))())))())()(())())))))))()())()))(()()(())())))()(()((()()((()()(((((())(()())()))(())()))(())))(())))))))()))()))((()))()))()))))))))()))))))))((()()())(()))(((()))(())))()))((())(((())))()())))())))))((())))))(())())((((((())())()(()))()(()((()())))((())()(()(()))))(())(()()())(())))())((()(((())())))(((()())())))())()(())())((((()()))))())((()))()()()()(())(((((((()()()((()))())(()())))(())())((((()()(()))))()((())))((())()))()(((()))())))()))((()(()))(())(()((((())((((()()(()()))(((())(()))))((((()(()))(())))))((()))(()))((()(((()(()))(()(()((()(())(()(()(()(()()((()))())(((())(()(()))))(()))()()))(())))(())()(((())(()))()((((()()))))())(()))))((())()((((()(((()))())())(((()))()())((())(())())(())()(())()(()()((((((()()))))()()(((()()))))()())()(((()(()))(()(()())(()(()))))(((((()(((())())))))(((((()((()()((())())((((((()(())(()()((()()()()()()()(()()))()(((()))()))(((((((())(((()((()())()((((())(((()(())))()((()(()()()((())((()())()))()))())))())((((((()))(()(()()()))(()((()(()(()))()((()(((()()()((())(((((())()(()))())())((()(())))(()(()())(())((())())())(((()()()(())))))())(()))))))()))))))())((()()()))((()((((((()))(((()((((()()()(((()))())()(()()(((()((()()()()())()()))()()()(()(())((()))))(()))())))))))()(()()(((((())()(()(((((()((()(()()())(()((((((((()((((((())()((((()()()((()((()((((((()))((())))))))())()))((()(()))()(()()(()((())((()()((((((((((((()())(()()()))((((()((((((())(()))())(()()((()()))()(((((((()((()()((((((()(((())))((())))((((((((()()(((((((())(((((()())(((())((())()((((()(((((((()(()(((()((((((()(((()(((((((((((()()((()()(()))((()()(((()(((())))((((())()(()(((())()(()(((())(((((((((((()))())))((((((())((()()((((()())())((((()()))((())(((((()(()()(()()()((())(()((()()((((()(((((()((()(()((((()())((((((()(((((()()(()(()((((())))(())(())(())((((()(()()((((()((((()()((()((((((())))(((((()))))()))(()((((((((()(((())())(((())))(()(()((())(((()((()()(((((()((()()(((())()(()))(((((((())(()(((((()))((()((()((()))(())())((((()((((())()(()))(((()(((((((((((((((())(((((((((()))(((()(()()()()((((((()((())()((((((((()(())(((((((((((()(()((())()((()()(()(()()((((()()((())(()((()()(()()((((()(((((((())))((((())(())()(((()()((()()((((()((()(((()((())(((()()()((((()((((()()(()(()((((((((())(()(((((())(()())(((((((()())()(()((((()((())(()()())((((()()(((()((((())(())(()()(((((((((()()))()(((())(()(()((((((())(()()())(()))()()(((()(((()((())(()(((((((()(()(()((()(((((()(()((()(()((((((()((((()()((((()(((()((())(()(()((()()((((()()(())()(())(((())(()((((((((()())(((((((((()(())()((((())))()))()()(((((()()((((((())(()()(((()(()(((((((()(()(((((((())(())((((()((()(())))((((()()())(()))((()())((((()(((((()(()(())(()(()()())(((((()(((((()((((()()((((((((()()))(()((((((())((((())()(()(((()()()(((()(()(())(())(((((()(())())((((())(())(()(((()(((((())((((())())((()(((((((()(((())(()(()))(((((((((()((()((()()(()((((())(((()((())((((())(()(((()(((()(()((((()(((())(()(((()(()()(()(()((()()(()())(())())((()(()(((()(((()(((()()(((((((((()(((((((((()()(((()(((()())((((()(()(((()()()((())((((((((((())(()(((()((((()())((((()((()))(((()()()(((((()(((((((())((()())(()((((())((((((((())(()((()((((((((((()()((()((()()))(((()())()())()(((()())()()(()(()(((((((())()))(())()))())()()((())()((()((((()((()((())(((((()((((((()(())))(()))())(((()))((()()(()(((()))((((())()(((()))))()(()(())()(((((())(()(()(())(())()((()()()((((()(())((()())(()(()))(()(()(()()(())()()(()((())()((()))))()))((()(()()()()((()())(()))())()(()(((((((((())())((()((()((((((())()((((())(((())((()(()()()((())(()((())(((()((((()()((()(()(((((())()))()((((((()))((())(((()()))(((())(())()))(((((((())(())())()(())(((((()))()((()))()(()()((()()()()()())((((((("

    //#endregion

    let day02data =
        """
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

    let day03input =
        "^><^>>>^<^v<v^^vv^><<^<><<vv^<>^<^v>^vv<>v><vv^^<>>^^^v<<vv><<^>^<^v<^>^v><<<v^<v<<<v<<vv<v<^><^>><>v>v^<<v^^<^v<><^>^<<^^^>v>>v^^<v>>^>vv><v>>^>>v^>^v>^<^^v>^>^^v<v>^^<v<>>v^^v><^><^<<>v^<^<^v<v>v^>>>v^v^>^<>^v<^^vv<v>^>^<>^^<vv^<><<v<^<^^>vv<>^>v<^>^v>v^>^v<>^><>><vv<>v^v<><>v^v>>>>v^^>^><^^<v<^><^<v>>^v^v<>v<<<^<<vvvv<<v^vv^>v^^^<^^^<v>>v<^v>>>>>v<^^^^>v<^<><v>>>>><v>>v^vvvv^^<v^<>^v<^v^>v><^>^v<<>>vv^>v>v^^>vv^<^vvv<>><>><><^^^<v<>^<^^^<v><^v>>v>^v<v^vv^<>^^^>v^^^v>>^v^^<^>>^>^<<v>>>^^<>>^vv>v^<^>>>><v<><><^^v<><<<<^^<>>^<vvv^><>v<v<<<<><v<<v>v<v^><vv<v^>^<^>v^^><^v>^^>v<>^v^<>^vv^><v^^vv>vvv>v>^<vv^>>^>>^>><>>>^^^^v<vv>^<>v^^><v^>^<>v<^^v><v<<><^v><>^^^^^v^v>>^^v><<><<vv>^^^^><^>v>><<<^v>v^^>^v^<^^v>v<^<<>>^v<<<v<<>>v<^v^><vv<v^v>v^<v>><v>^v<<<vv^>v<v>>v>>v><v><v^>v^^v>^v^>>>><>^>v>^v^>>>>v^<<vv<^v><<>v<v^<^^<<v<^v^^v^>vv><vv<v^<^>><^^>^<><^^<v<><^v^v^<^^>^<v><^<v>v^<<<^^v<v>^v>>><>^^>vv<<^v^<<<<^^>>>v>v<<<>^^>>>v>^>v>vv<<>^<^><v^>^^<^<v<<v<^>>^v^<vvv><>v^><<v>^^<v^vv^^^<vvv^<^>^>vv>><^v<^<<v<><<><<^^<><><vv>v>^<v>>^<>>^^v>vv^<^^v>><^vv^<<v^^><<>vv<v<><v<><v^^^v^v>^v<^<>v^^>><>^<^<v^<v^v^>v<<<^<<^>>>^^<^^v>v^<v>vvvv>v<>><^>^<<<<v^<v<>v^^^v<>v>^<v<<^^v^^<>^<<v^^<^<v>v>>v>>v^>^<vv<<<<<^<><>v><>>>v^>^v<^<><<v<^v^^<^<><^>^^^>^><>^><<vv>^<>vv<<v^v<<<<<>>>v<vv>^v>^>^>^<^><>v<><>>>^^<v>^<^v>>^<><v^><v^>>>v<v^^vvv^><v<v>v^>vvvv>>><^>v<>^^^>v>>v^<v<>v^>^<v^>^<<^>^>>v<<><<v^^>>v^<v^<^v^>^>v^><<^<v>v^<v>>^^<<v>v><<<^v^<>^<>^>>^<<v>^^<>^v<>v^>>><<v>><v^>^><v^<><v><>><v^<>vv>v^<^^^>v>^^<vv>>^v<><>>><>><^<>>v>v^^>^^<^^>^>>v>vv^^v<^<^v><vv<v<^>><<vvv<<><^>^v>^^^<<>v^<v<v><<v>^^v<<<>^^vv<^>vv>^>^<><<>vv<^>v^vv>^^^v><<^vv>^v<><v^^^^v^>vv^^<^<>^^v^<^vv<v<vv<>v>v^^<>^^>^^>^<><<^v>^><^^vvvv<><>^<v^^>v<>^><>v>><>vv^<<><<>><>v<^>^v>>^^v><<<>>^<^v^<v<<<v^>^^<^<><><^><<<<^<vv><v<<><vvv^^><vv>^<<vv<<<^v<>>><><>>v><<<v>vvvv^^vv<v>><<^v^vvv><><vv>v><>v<<<^<v^>><^^>v^<v>><v>^^^v^v>><<<v<^^>>^v<>v^<vv^^<<v<v>v<<<<^^^v^v<<>>>v>>vv>^^<><^v<v><>>v^>>>>>^>v^v^<^v^v^vvv>v<v<^>vv^<<v>vv>>v^^vv<^v>>>>vv<>v<>^^vv^<v>v^>>vvv<<<v<<^vv^^^^>v>v>^><<<^>v^><v<^<<<v>^v^^^><<><<<^^<^^<>^<v>^<v<<v<^^vv>v<^v><v><v<>^v<^<v<^<v^v><v>><v<v<<>^<v<>>><>^v^v<<^><v^<<v<v^>^>v><^>^vv^^<v<v<vv<v>^v^v^>^<<>>>>>v^<>^>v^vv^><<>>^^<>v^><v>^vvv^>v^v><>^><<>v>v<^<^><^^vv<<><>>v>>v><vv>>^v<<>^vv<>^vv>v>v>^>^>>><><<>v<v>^<<^v^^<<<><v>>vv<^<vv<vv^<<v<<^v><<>v<^^^<<^v^>^v>^^^v^v>>>v>v^v>^>^vv<^^<<vv^>^<<<vv>v^<><<^vvv^^><>vv^v>v>^><<^^^^vvv^<vvv>><^v<^>^<>>^<v<<vv>>><v>vv^<>><v^<v>^v>^>v>^<^<^^^<<vvvv^>>>>>>>v><vv>^<>^^v^><>><^v^^<v^v<<<<v^>><>v^v<vv<><^<<<<^>^^>vv>><^v<v^v<<>^vvv>v^^><^^<^<>>^^v^vv<>v<^<<<v^^^><v<vv<<>v>v<>^v^><v^vv^v^^v<^^v^^v><>v<^v>><<^<^v^>><<vv<<^>^<<v^<>^><>v><vv^v>>^<v<<<^>vv<^v>^>v<<v>^>>^>>v^<v<v>>^v<^v^v><<><>^><<<><v<vvvv<v^<v^v><>^<>^^^^v>^>^vvvvv>v>>v><<vv<<v<><<^><<^v><<v<<<v><vv<^>^v>>>>^v<^v<<>>^>^<<vv^<^>v>><<^>^>^v><><>^><<v<>v^><<^v^<^^><^^v^<<^v^^>>^v^<^><vv>v^^<<^^^<><>^>v^v>v^>^v^vv>^^>>>>^^<^>>>^^v<vv<><^^<vvv<^^^vv>v<v<v>><<<>^>^^>^>^v<<<<>>^<<>><v>>v>^^<^v<>v<>v^>v^><^<^^><v^^v>^^vv<v<<>><<vv<>>v>^<<<<v<<v>^><^^<^<^<v^<<^^v>^v<^>v^v^<v^vv^>^^><^>v^v>>^^v^><vv<v<v<v>>>>><<><v><v^v^<v^<^^<v<>^>v>v<>>>v>^^^^>><v^v^^v<<<>v^<<^<v>>>><^v^<<><v<>>v><><v<v^v>^v^^<v<^<^^v>><<vv<<vv><>>^>^>vv<^<>^vvv^v<v^^<>v^v>^^<<<<<>^v^>^<>v^^<>v^v<vv>^<>vv^<^vv>><v^^vvvvv>><<>v<vv^<^<vv^v^<>^^<v^<vv^<v^v^v<<^>^>^>^^>>>vvv>^>v>v>>>^>vv^><>^><>v>^^<v^>^><<v>><<<>>v<vvvv^>^v<^<>^<v>^<>^^<<><>^v<><>>>^vv<^<<^<^v>v<<<<<^^v<^v<><v<<><^>v>^v>>^v^><^^^^v<><><>vv^<>vv<^v<^^><v^<^><^^v^v^<^^<<><v>v<v<v^<<^v><>v^v<^>vvv><<^v>>v><><v<<^>>>v<^>>v>^<>><>^<v^v^<vv<<^>v<^^>^<^v<^<<^^v<>>^>^>^v^^v^v<v^^vv^<v>>v><vv^vv>v<>v^>v^^>^^>><v><v^<<><<>><<^^>><^v<v<><<><<><v<v^<^<v>>>><v^^v^^>>>^^^^^<<vv<^><>^<<<vv^^^>^><<<v<^v>^<v<^>^vvv<<>vv><<>v>v^v>>>>>^<>><^^^><<<<v><<vv>>>v<^<vv^v^<<v>>>>^^vvv>v<>><v>>>v>>^v^vvv<<>vvv<<^^^<>vv^^v<<>^^^>>^<^v^<^^>v^><v>>^<<^v<<vv<vv>v^>>^>v^><^><>^>>>vv>><^^^>vv<<^^vv><^<>^>^^<^<>>^vv^>>^v><>v^>>><<<^^<^>^>v<^>^<^^<>>><^^<>^v^<<vvv<v><>vvv><v>v^v<<^<v>^^><<^vv^v>v>v<<^v^<<<>^><><vvv>v>^vv^v<>vv^>^^<^>^>v^^<vv^>v><v<<<><>>^v<^<><><^<v^^<<^<v>vv<><<>v^<v^>^>^^<><<>^<^<<v^^v<v^<><<>v>><^<<>^>^v^v<v^v><^>>^v<^>v<<>^^^<^v>>>^<v>vvvv<<v^<^^>vvvv>v<>v<v><vvvvv>^<><>vvv<>^<<>^>>>>v^<^<><^v>v^>>v><>^><<v^>^<<>^>^v^<v^^>>^v><v>^<v><>v^<^^>v>^>>>v^v>>>^<>^<>>>>>v>>vv^v<><<<><><v><<vv<<v<><>>vv<^<vv>^v<<>v^v<^v<><v>>^v>>vvv^^v>>v>^>^>v><v><^>^^<<>^v<^<<<<^>v<^>>v^<^v>^v<<>^>^vvv<^^vv>^vv>vv<>>v>v<v>>v^<<<<<^^v^>v>^<<<v^v>>v<v><vvv><v>^<vv><<>>^<^>^^<>>>>^<^v<>v^^>^<^^v<^><>><v>>^v^vv<^v<^><<vvv<>><>><^^>^<^v^<^<>v<<<^v>v^^^<>v^<v^>^v^>><>^^<v<^><<^^v^<>^<^vv>>><^v><v^>vv<^v<<<v^>>v>v^v>^<v>v<^<>v^vvv>^vv<<<<v><^><v>>^^>><^v><<^>v^^<<v^^<^<><<<<>^<v<^v^>v<<^^>v<<<<<vvv<v<^>^>^>^>>^>>>v^<<v>>^^v><vv<^v<v<^^^>>>^vvv<^v<>>>vv>^^><^v>vv^>>v>v^<>^<vv>^>^<<^>^^^>>^vv>^^>vvvv<>>^^^^>>>v>v^^>vv>vv^<<>^><^<v^vvvv><v<><v>><<<v<v<<^v><vv^vv^<>>>^>^<v<^v<>><^<vv^^><v>v^>v^<><v^vvv>^>v^^v^>^^>v<<<<^<<^>>v>v^^^<<<v>>>^^v>v<v><<<<^^^v>^vv^>><>^v<v<<^^<<<<><>>>v>vvv^v^^v^>>vv>^>><>^v><^v^><^^>vv>^<^<^>><v>v>><><><v>^>^>v>vv>vv>^^>v>v^><v<<v^<>^>^v>^^v>^<^v<>>vvv^^>^>vv<v<v<<^<^<v^<>v^^v<^<^>vv^^<v><^^^>v>vv<<v>v<<v^<v^^><vv>^>^v^<^>v<^>^<>vv^><v<^><>>^>>^<^><<>^<^>v>v><>>>^<<^><<v><^v<v><>>vv<^><v^>>v>v>>>>^^>v<^v^>><<^<>>v><^><<^>^<vv^^<><<>><vvvv^>^^<><^^v>^^>vv>^v<v>>^^v^<v<^><^<<>>v^^^<^><^<<><<v<>><<>^v>vvv^vvv^^>>^<^<v>><>^<<<<^^<>>>v^<<^^v>><><<v<^>v>^v<v^>v>vv^><>^><<><^^>^>^<><>><^^<v^v<^><><><v>^<v<<v^<<^^^v<v<^v<>>><^v<<<<>>^v>^^vv^v^<<v>><<<v>vv>>v>>^v^<>>vv^<^>^<<>v<<<^vv<^vv^vv<^v^^^<vv^>v>>v<^^<^^vvv<^^v<>>>^>v^><v>^^><>vv>v>v<<<^^v<^vv^v>^^^>>>^^<>^^<^vvv>><><<><^<v>><<>^>^^<v^v^>vv>vv<v>^^<^^<<><><<v><v^^>v><v><<>v>vvv<^^^^<^>>><<<^^^<^>vv^^v>>v<<v^^<vv^<^>vvv^^v^^<^<vv>v<^<>^<<vv^^>^v>>^><><>v<v<v<>><v>>>^^>>v^><v^^<^>><>v<><<v^v<v<<>>>><>>>>><<^vvv<<><><<>^><><<^^v><<^>v>^>^v>v>>^^<><^>vv<^<^v>v<><^<<v<><^><>^^^<v^<><vvv^^^<>^^v><v<<<v>><>^>^vv<v^<vv>v>v^vv<v^v<v>^v^>v><>v^><>v>^^^^><<vv^><v<<v<^<>^v^^^>^^><<<v<^<v^>^^>v><vvvvv^<^<v^^>v<^v^^vv^<<<<v><^>v>v^v><><v^<<^<<v<^^^>^><v^v^<><><>^v<v>^<>^v>^v>v^<><^><v>>v<<^><^vv^<><^<>><>><v<v><<^^^^>v<^<^vv<><^vv><<^<<v>v^>>^v>^>v^^v>vv<v>v<<v>v<>^>>vv^>>><>^v^^<^>v<<^<^^v^^v^<<v<<v<^v<>vv^<v>><^v<^>>>vv^^<v^<>^^v<v<v>>^><^^^<><<^^>v<<vv>><<vvv>><<v^v^>><>vv^><<^>^><^v<^<^<vv<^^vv>v^v<<<<<<><<vv^vv>vv>v<^><<><><<>>v>><v><^>^v>^v^<>v^^^><^^<<<^vv^vv>^v^vvv^^>v^<v>><^<^<^<>^vv<vv^v^^>^^^>vv^v>>><<<^<>>v>v<^^<><v>>><><^v^^<<><<<>^<^^v^>v<vv^^^^>><v><^<<v<<v<>^>^>>^<>^v><>>^<v<vv^<<^<<>vv^>^^<<<^v<>>^v<>vvv<<^^<<><vvvvv<<^<^^<>>>>^^<><>^><>^v<v^^v<<v^^<^<^>v<v>^v<^>^v<>v^vv<><<v>^vvv<><<^>>^^><><>^<>^>v^^v^><v<><>>v><v^<v<<v>><^v>^<v<^>v<<<>vvv^<^^v<vvv^vv<>^<>^>>v<>^^><><v>>^><^^vv>><<>><v><^><>>^vv>v<vv<>v^v^^v<<^^<vv>v^^vv<<^<<><>^<><v^><^<^<>>^vv<v>v>>^<^vv>^vv^>v>^<><^><^<>v^v^^<^<>^^v>>><^v<>v^v<<^>v><>^^<<v^v<>v^>>v>^<><vv^v<v^<vv<>^>^>^<^>v><<><><><<<>^>><v^^><^>><v>>^v<<<^<<>^><<^>>>>>v<^>v>>v^<v^>^>v^^><>v^v^vvvv<v<v<>v>>><<>^<<vvv><v^v^>v<v^^^>>^<v>>^vv^^<vv><^>>v<v^><vvv<^^>>vv^v<^<>^v^<<v>^<<><<<^vvv^>^^<<>>><v<^>vv<<^<><^v<^<><<^^>vv^v>v^^^>>>>^>vv<<v>v>>^^v^^><>v<<^><^<v^>>^>v^v>><^v^>v<<^<v><^<^<^<>>v^^>><<<>v<v>v<^^>^vv<<<^^<v<>v^^>v<<><^<>^^>^v<>v>><^^^vv^>^><>v^^<v^<>>^<v^^^><v<><vvv>v>^<<^v>^>>>>><^^^<>v<v>>v^^<^v^>>v^<<v^>^>v^v>>>>^>>vv<>^<^v><v^^<>v>v^v>^<>^>v<vv><<v<^v<<^v<<^v^vv<><>^<>>^<>>^<>v^><<>^v>>^^^^<<^v><>^<^>^^v><^^<^<v^<^^v>^v><vv>v<<^>^>><<^^^vvv<<^vv<^^>v^^vv^<^^<<^^>>^^<vv<v<<v^^<<v<^vvv<<><<v>v^>>v^^>v<^>^><v<^>v<v^v<v^^<>v>><<v^v^v<^^^><v>v><^<^vv>^^v>^>v<<^vv><^^^^^^><<^>>>^v<>^^v<<<>><<<v^><>^<<<v>v^>^^^<^><v>^^^v<<>v<v>^<v^>><<^^<<^v<<>^v>>vv>><v<^><v<<<vvv><vv><<^v^^<v^vvv<^v>>v^v<v^v^>>^^v<><^^^<^^>v>^<><v<<v^^>vvv^v^^<v<v^v>^>v^^v<^><v^^<<<<>^^>>^v<><^><^<<^vv^<><<>v^vv^<v^<><<<^^>v<<>>>v<>v<><<<v>^v>^^v>^^>v>^>^>v<>><>^>^>^vvvv<^<v^<>^^^^v>v>><<v>>^<vv>>^<v<^v^vv>><>^^>v^^<<><^<v>><<<<>v>^^><v^^v<<v<><vv^v>^<v^^>v<<<<v^v<<>>vv<v<<<v>v>>v<^v>>v>v^<<<>^>^>^<>v<^^vv><^v<<^v<vvv^vv>v<^<<^^vv^^>vv<^>v>^^<<v^<<^^v<>^>v<<^^<^>^^^v^^<v<^<^>>>v^vv^<^v>^<>^<^<v<^v>>>^<^v<><v<^vv<v>v><v^v^^v<vv><^^<><>^>v<^<^vv>><^v><v<>^<>^^>^<><<<v^>>^<>><<><v>vvv^<<^<vv<v><v<^<<<^>^>>v<^>>vv>^v^^^v<>v<>><>^vv^>vv^"

    let day05input =
        """
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

    let day06input =
        """turn off 660,55 through 986,197
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

    let day07input =
        """
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

    let day08input =
        """
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

    let day09input =
        """
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

    let day12input =
        """
[["green",[{"e":"green","a":77,"d":{"c":"violet","a":"yellow","b":"violet"},"c":"yellow","h":"red","b":144,"g":{"a":["yellow",-48,72,87,{"e":"violet","c":123,"a":101,"b":87,"d":"red","f":88},{"e":"red","c":2,"a":1,"g":"blue","b":"green","d":"violet","f":170},"orange",171,162]},"f":"orange","i":"orange"},49,[{"c":{"e":"violet","a":-44,"d":115,"c":117,"h":194,"b":{"e":-17,"a":172,"d":"green","c":197,"h":53,"b":106,"g":"violet","f":-10},"g":"red","f":"orange"},"a":-49,"b":["violet","orange","blue"]}],"green"]],["orange"],{"e":"blue","a":["red","yellow"],"d":{"a":[{"c":{"a":181,"b":["orange",-40,"red","orange","yellow",31,60,71,"yellow"]},"a":[114,-40],"b":"orange"},["green",93,10,{"c":11,"a":170,"b":[161,-3],"d":-16},58,{"e":{"c":-2,"a":117,"b":"violet"},"c":["blue","yellow","red","violet","yellow",123,113],"a":"orange","g":19,"b":108,"d":"red","f":"yellow"},{"e":"green","c":"yellow","a":{"e":28,"c":"red","a":"violet","b":"red","d":"green"},"g":"yellow","b":116,"d":148,"f":"red"},[15],["green","green",43],"blue"],[133],"green",134,"violet",{"c":"red","a":[71,41,"blue"],"b":"yellow","d":"violet"},132,[10,"violet",[182,"green","green","orange"],78,{"c":"blue","a":[100,-36,"blue","violet",-10,"orange"],"b":{"e":"orange","c":"blue","a":160,"g":"green","b":190,"d":"red","f":186}},16],[{"c":"green","a":"violet","b":20,"d":"red"},"green","blue",{"c":[0,84,184,"orange",-34,"blue","orange",0,"violet","violet"],"a":10,"b":89},"green",182,127,-2,196]]},"c":-20,"h":[[165,[180,"yellow",-5,16,"red",[{"e":"orange","a":"orange","d":"orange","c":"yellow","h":"red","b":182,"g":21,"f":"violet"},"red",69,"violet",10,"red","orange"]],[160,"blue",{"e":"yellow","c":"violet","a":"green","g":43,"b":[-40,"yellow","yellow",118,57,"green","violet","yellow","violet"],"d":"yellow","f":"blue"}],130,{"e":["yellow",58,"green",139,"violet","red"],"c":"green","a":"green","g":{"e":163,"c":33,"a":15,"b":78,"d":"green"},"b":86,"d":"orange","f":58},"red","red",{"a":37}],"yellow",{"e":44,"a":{"a":136,"b":"yellow"},"d":"yellow","j":39,"c":[-28,["violet",{"e":"red","a":150,"d":189,"c":76,"h":"orange","b":"yellow","g":164,"f":"orange"}],{"e":12,"a":"red","d":"yellow","c":"yellow","h":102,"b":"yellow","g":"red","f":147},"violet",[40,"red",24,193,105,179,"violet","green",{"a":"yellow","b":"violet"}],39,-25,{"a":127},126,{"e":"red","c":151,"a":-46,"b":"green","d":"violet"}],"h":"red","b":122,"g":93,"f":["violet",{"c":102,"a":-16,"b":39,"d":176},"red",187,{"e":"blue","a":172,"d":{"a":-5},"c":25,"h":{"a":"red"},"b":"blue","g":["yellow","red"],"f":{"e":158,"c":85,"a":"blue","g":"green","b":"violet","d":125,"f":93},"i":11},"violet"],"i":[195,{"a":"blue"},48,[44,25,"green","red","violet",172,"orange",49]]},["yellow",[{"e":-4,"a":"red","d":"green","c":"blue","h":"green","b":"green","g":190,"f":30,"i":-2},"green","violet",{"e":92,"c":-11,"a":{"e":"orange","c":0,"a":47,"g":"blue","b":"orange","d":"yellow","f":-47},"b":40,"d":193},97,"violet",[51,168,"violet",{"c":-45,"a":167,"b":"orange"},"blue","orange",64],[188,"green",91,-10,66,"green"],"green","blue"],"green",-42,{"e":"red","c":"red","a":199,"g":84,"b":"blue","d":["blue",194,["green",88,110,-23,"yellow","green",15,"violet"]],"f":"red"}],{"e":"blue","c":-33,"a":-34,"b":"blue","d":81}],"b":{"e":"violet","c":165,"a":"violet","b":{"a":"red","b":"yellow"},"d":[6,71,"orange","orange",{"a":96},74,"yellow"]},"g":["yellow",["orange",[-27],[[[8,"red","blue",-46,62,"yellow",94],[60,"violet",141,"green"],16,"green","yellow","blue",81,[93,"red",183,"blue","red",30,-16,"red","red","yellow"],-21,139],{"c":"blue","a":"orange","b":"violet","d":51}],"blue","yellow",["violet",19,"orange","yellow","red","violet","blue","violet",{"e":"yellow","a":"violet","d":137,"c":"blue","h":197,"b":"orange","g":{"e":"yellow","c":"orange","a":18,"b":42,"d":183,"f":142},"f":68}],"green",["blue",85,"violet"]]],"f":"green"},[92,{"c":-26,"a":{"c":["orange","orange",{"e":131,"c":192,"a":-38,"g":16,"b":27,"d":"yellow","f":-46},120,"orange",-28,-18,3],"a":["red",-15,{"e":56,"c":-15,"a":150,"g":"blue","b":"orange","d":"violet","f":70},-44,{"e":"green","c":53,"a":"blue","b":"blue","d":-34,"f":"violet"},"orange",[161,"orange",-19,{"e":27,"c":98,"a":"violet","g":"yellow","b":-45,"d":191,"f":"green"}]],"b":-41,"d":[["orange","red","yellow",120,140],[{"e":-1,"c":"orange","a":"yellow","b":"yellow","d":86},42,[78,140,"orange","green","orange",-49,159],"yellow","red",90,{"c":"green","a":62,"b":"violet"}],"violet",178,30,"green",186]},"b":"red","d":"violet"},{"e":-21,"a":{"e":{"e":{"e":"orange","c":115,"a":199,"b":-43,"d":"yellow"},"a":"blue","d":"green","c":"blue","h":"violet","b":"red","g":"violet","f":{"a":5}},"a":[{"e":"green","a":[20,"yellow",23,100,"orange",142,"red",-3],"d":19,"c":18,"h":"blue","b":"yellow","g":"yellow","f":{"a":-33}}],"d":"yellow","j":"yellow","c":["red",-2,95,-1,"green","violet",12],"h":"orange","b":{"e":[94,-14,"orange",68],"c":["blue","yellow",[-12,"yellow",126,"orange",199,"red",133],{"e":-14,"a":40,"d":-48,"c":5,"h":"orange","b":"red","g":"yellow","f":"red","i":"orange"},"blue",[-35,87,84,"yellow"],"red","red",86,"yellow"],"a":11,"g":{"e":-34,"a":"orange","d":"blue","j":"blue","c":"yellow","h":"yellow","b":"violet","g":42,"f":188,"i":53},"b":"yellow","d":{"e":"red","c":84,"a":"violet","b":"orange","d":87},"f":["blue","yellow","red",{"c":"orange","a":"blue","b":61},"violet",20,-22,129]},"g":{"e":28,"c":170,"a":["green",98,"orange",150,"orange"],"b":"blue","d":"blue"},"f":"blue","i":[{"e":"green","a":"blue","d":19,"c":177,"h":-18,"b":{"e":-19,"a":"green","d":"yellow","c":172,"h":"red","b":"red","g":"yellow","f":"yellow"},"g":{"c":-36,"a":"red","b":"violet"},"f":121},141,"violet",113,81]},"d":{"e":133,"a":19,"d":-27,"c":{"c":["yellow"],"a":105,"b":{"a":"orange"},"d":{"e":"yellow","c":"orange","a":"orange","b":163,"d":"violet","f":"red"}},"h":"yellow","b":161,"g":110,"f":[[137,6,[195,"violet",179,93,"green",130,"blue","yellow"],70,"orange",-8,-28,"orange",{"e":59,"a":"red","d":128,"j":88,"c":"violet","h":188,"b":0,"g":0,"f":"green","i":2}],86,"green","orange",68,"yellow"]},"c":{"e":"violet","a":-49,"d":["blue",140],"c":-29,"h":["red",4,-45,165,["yellow","blue","blue"]],"b":"blue","g":{"e":{"e":{"e":"blue","c":168,"a":51,"b":-28,"d":"orange","f":"violet"},"c":"violet","a":"green","g":"blue","b":-29,"d":121,"f":69},"a":"violet","d":["orange",[12,192,"green",-17,160,"blue"],131,"blue",41,{"e":"violet","a":"green","d":"blue","c":182,"h":"red","b":10,"g":"blue","f":-37,"i":151},"blue"],"c":{"e":"orange","a":182,"d":155,"j":18,"c":-41,"h":119,"b":148,"g":"green","f":104,"i":141},"h":16,"b":"blue","g":[137],"f":"green","i":-35},"f":[["green","red",19,"yellow","blue","red"],["red",37,[36,"red",-38,183,"violet",-17,119,93],130,-20,77,[64,115,66,"green"]],-13,-23,"green",100,"orange",{"a":"orange","b":"red"}]},"h":["red",{"e":28,"c":{"c":"green","a":149,"b":["orange",137,"violet",184,"orange","green","red",20,72]},"a":114,"g":["blue",{"e":"yellow","a":104,"d":"red","c":-17,"h":"blue","b":"violet","g":"orange","f":"red","i":"red"},181,21,"blue","orange",111,{"e":39,"a":"orange","d":196,"j":119,"c":143,"h":-42,"b":"green","g":190,"f":-43,"i":37}],"b":"orange","d":{"e":["green",44,"green",177,"violet",-44,160,"violet",85,95],"a":{"e":138,"c":"yellow","a":"yellow","b":"blue","d":"green"},"d":"violet","j":"blue","c":100,"h":"blue","b":104,"g":-28,"f":189,"i":"orange"},"f":[121,{"c":110,"a":68,"b":5,"d":57},[108],15,"red",[83,"blue","green",-16,"yellow"],"green"]},[[80],160,68,187,"green","green",94,113,2,163],34,"orange",["yellow",{"e":72,"c":["green",11,"green","green","orange"],"a":{"c":"violet","a":13,"b":66,"d":36},"g":"blue","b":"yellow","d":140,"f":145},"red",53,-11,"yellow","blue",148,{"e":"violet","a":"violet","d":83,"c":"yellow","h":103,"b":-23,"g":36,"f":[154,"red",62,112,35],"i":"violet"}],{"e":174,"a":-49,"d":58,"j":114,"c":"violet","h":[162,"red",54,-8,[142,178,"red",26,"violet",71,-20,38,"orange"],"orange","yellow",[49,25,"violet","green","blue",189,"green","yellow",-25,55],"red"],"b":-13,"g":156,"f":[48,107,-15,167],"i":"blue"},["red",{"a":"red","b":"red"},"violet",[73,"blue","violet","red"],["red","red"],63,-12,108]],"b":{"c":"violet","a":{"a":-39,"b":166},"b":183},"g":36,"f":[45,"yellow","blue","violet",26,7,[[156,-18,"yellow"],-4,-37,[129,-11,["yellow",179,"violet","red","yellow","violet"],"red","red",133],17,"green",137,"red",-9,"yellow"],"yellow",[{"e":"green","a":"yellow","d":52,"c":127,"h":132,"b":38,"g":"yellow","f":"violet","i":"red"},"yellow","red",{"e":{"c":60,"a":126,"b":88},"a":159,"d":"red","j":70,"c":23,"h":195,"b":178,"g":"red","f":"yellow","i":"blue"},13,-37,[196,146,145,"orange",60,"violet",["red",144,51,"red",-26,172,"yellow","red",52,"yellow"],157,"green"],[{"a":-7,"b":"red"},123,{"e":"yellow","c":"orange","a":"orange","b":40,"d":"blue"},139,"green","red",48,{"e":165,"a":60,"d":83,"c":"yellow","h":186,"b":34,"g":"blue","f":178,"i":33},[116,"yellow",179,18,32]],"red"]]},{"c":[6,{"e":{"c":79,"a":82,"b":"orange"},"a":"blue","d":[38,"red",37,[12,134,139,"violet",102,60,"green",82,91],"orange",84],"j":"orange","c":["yellow","green","blue","violet",{"e":"orange","c":157,"a":"green","b":"blue","d":"violet","f":"blue"},64,["violet",176,-7,137,"red",57,"yellow"],"yellow",["blue",170,159],"orange"],"h":170,"b":[3],"g":"violet","f":"violet","i":186},{"e":{"e":"yellow","c":"red","a":"blue","g":["violet",104],"b":124,"d":42,"f":"violet"},"c":-43,"a":-28,"b":[6],"d":[0,97,{"e":-9,"a":"violet","d":31,"c":23,"h":40,"b":76,"g":"red","f":94},["violet"],124,68,"green",37]},[84,{"e":"yellow","c":18,"a":"orange","b":"blue","d":"red"},["yellow",66],81,"orange",-22,-10,"green",139],"red","yellow"],"a":25,"b":"yellow"},[{"e":25,"a":{"e":["violet",22,103,{"e":193,"c":"red","a":"yellow","g":"violet","b":"yellow","d":-33,"f":29},{"e":-10,"a":77,"d":"blue","c":-15,"h":74,"b":-4,"g":"orange","f":153},"yellow",176,94,"green",141],"c":"blue","a":146,"b":-26,"d":-7,"f":149},"d":"green","c":["violet"],"h":"orange","b":[[23,"violet","blue","violet","violet",-40],"orange","yellow"],"g":57,"f":[{"e":141,"a":"yellow","d":"red","c":138,"h":118,"b":{"a":"yellow","b":"red"},"g":133,"f":{"e":169,"c":"violet","a":"green","g":193,"b":"orange","d":"violet","f":-17},"i":"yellow"},72,"green","violet",[106,"red","red","red","yellow",180,"orange",{"a":"green","b":"green"},111,"blue"],147],"i":-7},[[62,65,158,"blue",86,"yellow"],[71,[168,179,"yellow","red","green"],-7],"violet"],"violet",["yellow","red","blue","orange",78,47,{"c":"orange","a":"violet","b":152},[-37,"yellow"],-11,6]],"red",{"e":"red","a":"orange","d":{"e":"red","a":10,"d":"blue","c":{"c":"green","a":91,"b":"yellow","d":-28},"h":158,"b":[["orange"]],"g":"blue","f":[[137,157,50,10,"blue",-12,"violet",76,76,80],[164,46,"orange",-23,{"a":"green"},"yellow","green","green","yellow",48],47]},"j":{"e":["orange",{"e":191,"c":"orange","a":25,"g":"red","b":"yellow","d":148,"f":"orange"},-34,"orange","orange",-36],"a":[{"e":198,"a":["blue",80,121,36,102],"d":{"e":"green","a":"blue","d":76,"j":"red","c":127,"h":"yellow","b":"yellow","g":"yellow","f":163,"i":"red"},"c":"yellow","h":73,"b":"red","g":"green","f":"red"},"violet",{"e":133,"a":"blue","d":"green","j":3,"c":"violet","h":144,"b":25,"g":"green","f":102,"i":"green"},{"e":{"a":"violet","b":"green"},"a":"green","d":[86],"c":"green","h":3,"b":{"c":"orange","a":"orange","b":"yellow","d":193},"g":-34,"f":-35,"i":"green"}],"d":[{"e":"violet","a":"blue","d":{"c":79,"a":"red","b":0,"d":"violet"},"c":77,"h":"violet","b":"green","g":-47,"f":"green"},-49,90],"j":"blue","c":"yellow","h":["violet","green",28,"green",97,"orange"],"b":53,"g":{"e":21,"a":{"c":42,"a":"blue","b":"red"},"d":"violet","c":142,"h":158,"b":"blue","g":["orange",197,"blue","green","yellow",-3,15,-38],"f":62},"f":{"a":32},"i":79},"c":{"e":[{"e":[125,"yellow",-43,"orange","red"],"c":3,"a":"orange","b":"orange","d":{"e":195,"c":16,"a":"yellow","b":94,"d":-20,"f":-13},"f":"yellow"},89,["orange"],"violet"],"c":{"e":"red","a":"orange","d":5,"j":{"a":92,"b":142},"c":22,"h":"blue","b":"orange","g":{"a":[-13,199,"green",133,-41,-22,"orange",169],"b":[134,"blue"]},"f":183,"i":["green",-24,"violet"]},"a":116,"g":[{"e":57,"a":["blue",144,44,43,"orange",34,"yellow",126,"red"],"d":"green","c":"violet","h":"orange","b":"violet","g":"violet","f":[196,60],"i":-21},[168],["red","violet","yellow","green","yellow","green","blue",113,{"e":"orange","a":"red","d":-7,"c":-29,"h":"orange","b":-44,"g":"red","f":-32}],{"a":116},"blue"],"b":{"c":{"e":-41,"a":"orange","d":154,"c":"yellow","h":-12,"b":"yellow","g":"blue","f":"violet","i":105},"a":-23,"b":2},"d":161,"f":"orange"},"h":[187,{"c":"yellow","a":"orange","b":"orange","d":"red"},[[58,"blue"],[111,["yellow","green","green","violet","green"],"orange","blue",112,-45],31,"violet"]],"b":{"e":[[13,"blue",-19,"blue","yellow",144,23,17,110],"violet",{"c":"orange","a":"yellow","b":73,"d":"red"},9,115,"blue","violet","yellow","blue","green"],"a":[-9,"yellow","violet",183,"red",14,"blue",192,"yellow",165],"d":"blue","j":{"e":[86],"a":-14,"d":"yellow","j":"violet","c":"violet","h":{"e":85,"a":["red","yellow",114,111,129,37,71,"blue"],"d":"yellow","c":43,"h":11,"b":72,"g":128,"f":"red"},"b":11,"g":183,"f":34,"i":187},"c":122,"h":{"c":[93,132,"yellow","yellow",91],"a":"green","b":"orange"},"b":[{"e":"blue","a":"green","d":"blue","j":{"e":"orange","a":"violet","d":"orange","j":"yellow","c":"yellow","h":"orange","b":"green","g":"orange","f":"green","i":"green"},"c":186,"h":"yellow","b":145,"g":112,"f":"orange","i":"orange"},100,139,-11,{"e":103,"c":["green","red",-40,90,"violet","violet","yellow"],"a":140,"g":"red","b":"violet","d":"red","f":"blue"},{"c":-43,"a":"orange","b":66},"red",["red","orange",["blue",187],76,192,50,"yellow","violet"]],"g":38,"f":"blue","i":[129,[30,"green",157,92,181,176],{"e":"violet","a":127,"d":172,"j":"yellow","c":148,"h":171,"b":"yellow","g":{"e":115,"a":"red","d":48,"c":-12,"h":"blue","b":"orange","g":"red","f":78},"f":135,"i":79}]},"g":[[173,131,"yellow",193,162,"yellow"],[-5,{"e":[116,102,"orange","yellow"],"a":189,"d":136,"c":{"e":72,"a":"blue","d":-13,"j":"yellow","c":90,"h":"violet","b":169,"g":"orange","f":"blue","i":"blue"},"h":186,"b":"orange","g":"red","f":"orange"},{"e":-8,"a":-37,"d":104,"c":"violet","h":"orange","b":-31,"g":25,"f":168,"i":119},"green",32,[[197,"orange","violet"]],"yellow",{"e":["red",-16,"yellow"],"a":"blue","d":31,"c":"yellow","h":"red","b":"red","g":"violet","f":20,"i":"violet"},31,80],22,{"e":{"e":"red","a":125,"d":"yellow","j":111,"c":34,"h":193,"b":100,"g":"orange","f":31,"i":15},"c":{"e":75,"a":13,"d":-29,"c":["green"],"h":-46,"b":{"a":58},"g":100,"f":{"c":"violet","a":"red","b":-35},"i":["violet","green","orange","violet",183,0,-27,96]},"a":"red","b":[95,"orange","blue","green",170,{"e":3,"a":"blue","d":125,"j":-25,"c":10,"h":25,"b":"blue","g":182,"f":141,"i":27},["violet",7,76,-37,"red",59,"yellow",29]],"d":78,"f":88}],"f":["violet",72],"i":[96,["green","orange",63,"red",83,"yellow"],[{"e":"red","a":{"e":"orange","a":"green","d":183,"c":"orange","h":"yellow","b":146,"g":-1,"f":"red","i":"orange"},"d":"green","j":"red","c":"red","h":"yellow","b":"violet","g":-9,"f":182,"i":"red"},-49,17,"orange",187,-2,[178,"red","red",131,195,[94,-26,"blue","green",0,1,101]]],-25,14,"violet",{"c":"blue","a":"green","b":"orange"},198,-2]},9],[{"e":["green",177,[-38],{"e":"green","a":[147,"green",[56,93,"violet","red"],82,{"e":"blue","a":"orange","d":"red","c":30,"h":"blue","b":10,"g":"orange","f":"orange","i":82},193],"d":"violet","j":69,"c":"green","h":161,"b":-12,"g":{"e":125,"c":-33,"a":-42,"g":70,"b":{"c":81,"a":52,"b":"red","d":"violet"},"d":["violet"],"f":39},"f":["red","green",74,158,14],"i":"yellow"},[144,88,["yellow","violet",-1,"blue",109,[53,86,-36,91,"violet","green",59,15],171,"blue"]],185],"c":{"e":{"a":92},"a":67,"d":"violet","c":"blue","h":[71,"violet",25,154,{"e":16,"a":"red","d":"red","j":"violet","c":54,"h":"violet","b":160,"g":"orange","f":{"e":-47,"a":"green","d":"blue","c":56,"h":175,"b":118,"g":97,"f":"red"},"i":"yellow"},{"e":[107,"violet","violet","blue",-4,"blue","green",82,"red"],"c":{"e":"yellow","c":"violet","a":"orange","b":"blue","d":"green"},"a":172,"b":131,"d":"green","f":43},"red","green",["red","green","violet","violet",132,"green",153,195,-41,[128]]],"b":"yellow","g":"green","f":{"c":72,"a":{"c":"green","a":"violet","b":"green","d":180},"b":48,"d":["blue",70,60,"orange",139,183,"red","red",{"a":"red","b":123},"yellow"]},"i":[66,77,"green","violet",25,[193,"orange",78,"red",["violet","red",163,37,"yellow"]]]},"a":"red","b":175,"d":0,"f":[{"e":38,"a":"yellow","d":"violet","c":68,"h":{"e":"orange","c":129,"a":"blue","b":"green","d":106,"f":"orange"},"b":"red","g":"green","f":{"e":91,"c":46,"a":"blue","g":"red","b":"yellow","d":92,"f":"yellow"}},["green",65,150,86,"orange"],"green",{"c":"green","a":30,"b":"yellow"}]},["blue",70,143,{"a":"green","b":[{"e":83,"c":63,"a":-2,"g":{"e":"green","c":"orange","a":-46,"b":"yellow","d":"red"},"b":39,"d":"red","f":123},"orange",57,34,{"c":"yellow","a":{"c":"blue","a":"green","b":"blue"},"b":"orange","d":{"e":"blue","a":158,"d":"red","c":69,"h":122,"b":6,"g":93,"f":"yellow","i":163}},{"e":183,"c":99,"a":"orange","g":76,"b":42,"d":31,"f":118}]},{"e":31,"c":["orange",186,58,{"e":"violet","c":9,"a":115,"b":[115,"yellow",19,"violet","blue","yellow"],"d":106}],"a":{"e":"red","c":{"a":82,"b":180},"a":71,"b":"yellow","d":100},"g":{"c":68,"a":"red","b":{"a":"blue","b":70}},"b":"yellow","d":"violet","f":-4},"yellow",[{"a":"yellow"},[{"e":"violet","a":159,"d":"violet","c":"blue","h":{"a":195,"b":-16},"b":97,"g":74,"f":126,"i":83},-49,"orange","orange",20,{"e":-37,"c":82,"a":"blue","b":"yellow","d":"orange"},"violet","green",5],"blue",{"a":-2},{"e":-39,"c":"yellow","a":-3,"b":127,"d":[196]},{"c":"red","a":-1,"b":"orange","d":166},{"e":{"e":"red","a":97,"d":"orange","j":47,"c":84,"h":-36,"b":-5,"g":"red","f":"yellow","i":113},"c":55,"a":{"e":[13,108,137,"green","green",-9,71,-36,"orange","blue"],"a":"violet","d":95,"c":6,"h":125,"b":"orange","g":"orange","f":130},"b":-28,"d":[97,46,[-7,"violet",146,155,166,"orange","orange","yellow",148,"red"],"orange",40,"red"]}],[{"c":"violet","a":"yellow","b":64,"d":"orange"},{"e":{"e":"violet","c":"blue","a":{"e":"yellow","a":-41,"d":181,"c":101,"h":"orange","b":"orange","g":"blue","f":51},"g":-25,"b":"red","d":41,"f":1},"a":25,"d":{"e":"orange","a":"green","d":-9,"c":"orange","h":71,"b":"red","g":137,"f":133},"j":[["yellow",116],93,"orange","violet","blue",150,34],"c":66,"h":"violet","b":-49,"g":[60,194,[136,-37,160,"red","orange","red",179,"red"]],"f":[-24,"violet",35],"i":"blue"},{"e":92,"c":"blue","a":"red","b":"blue","d":"green"},"red",[126],96,"red",198],87],{"e":{"e":["orange","violet",{"e":"green","a":-42,"d":103,"c":["violet",-48,37,122,107,"orange","blue",97],"h":"blue","b":92,"g":"orange","f":0},"blue",197,-9,"yellow",{"a":["orange","blue",186,"blue","green","red","red",48,"red","green"],"b":195},121,"blue"],"a":96,"d":"orange","j":94,"c":66,"h":{"a":["violet"],"b":"orange"},"b":"violet","g":191,"f":{"e":"red","c":-32,"a":[149,[69,"green",84,25,"red"],"yellow","violet",4,"violet","green",69],"b":"blue","d":148,"f":111},"i":93},"a":[181],"d":{"e":{"e":{"e":"red","c":36,"a":143,"b":82,"d":11},"a":168,"d":"orange","j":-45,"c":159,"h":"red","b":{"e":120,"a":-37,"d":"green","c":"green","h":"red","b":59,"g":"violet","f":173},"g":166,"f":"orange","i":"yellow"},"a":158,"d":"green","c":126,"h":[[159,"violet","violet","green",101,"orange",141],"violet",122,"yellow","red",79],"b":13,"g":"red","f":{"a":"orange"},"i":{"a":89,"b":{"e":[-20,"green",6,58,18],"a":-17,"d":137,"c":[-25,"orange",95,"yellow","green"],"h":3,"b":"violet","g":26,"f":"green","i":168}}},"c":["orange",{"e":163,"a":{"a":6},"d":-25,"c":164,"h":[-47,"yellow","orange",[139,93,93,"yellow","violet","red",-12],"blue",-32,136,10],"b":"orange","g":"blue","f":174}],"h":["blue",-34,-29,{"e":"violet","a":3,"d":"green","j":"red","c":"orange","h":"green","b":"red","g":"green","f":124,"i":{"e":186,"c":"violet","a":168,"g":110,"b":[127,136,31,109,"blue","red","blue","violet",79,91],"d":"red","f":["violet",191,-15,-22]}}],"b":[124,{"e":-38,"a":{"a":"yellow"},"d":[130,{"c":158,"a":"blue","b":103,"d":197},-36,[153,-6,173,121,"yellow",94,168,"violet",77,-35],168,"red",{"e":-32,"a":"red","d":46,"c":82,"h":91,"b":"blue","g":"yellow","f":"orange","i":174},"green"],"j":[189,-43,41],"c":[185],"h":[182],"b":[139,"violet",-44],"g":"yellow","f":"red","i":["red",-18,"violet","red",31,"red",115,-49,["yellow","yellow","violet","blue","violet","violet"]]},143,"yellow",["violet","red",["blue","violet",{"a":-49},41,"orange","blue"],{"a":"orange"},[93,-8,"yellow",-39]],166,155,"red","violet","orange"],"g":"violet","f":"red","i":[85,126,{"e":{"e":"red","c":-42,"a":51,"b":"yellow","d":"red","f":{"e":130,"c":"violet","a":115,"g":"violet","b":-28,"d":-3,"f":"blue"}},"a":92,"d":114,"c":"violet","h":{"e":-41,"a":"red","d":57,"j":82,"c":"violet","h":"green","b":"red","g":2,"f":-20,"i":78},"b":"yellow","g":"violet","f":86,"i":67},147,146,-33,"blue","violet"]},{"e":[[{"a":174},21,"orange","green","blue",{"e":127,"c":{"c":0,"a":197,"b":"yellow"},"a":"blue","b":["yellow",153,9,"blue"],"d":136}],"blue"],"c":[{"c":"red","a":[-5,"green",["violet"],-47,19,173],"b":106,"d":"yellow"},182,[21,106,"violet",10,"green",20,"orange"],["green"],{"e":-25,"c":"blue","a":"violet","b":["red",27,"blue",21,193,"green",["green","green"]],"d":["orange"],"f":18},"yellow","yellow",{"e":{"e":"violet","a":"green","d":"violet","c":"red","h":171,"b":["red",149,"violet"],"g":"yellow","f":"blue","i":"green"},"c":[86,-30,"orange",56,123,"green"],"a":65,"b":[86,129,"yellow","blue",87,127,182],"d":4,"f":[-36,179,"red",-9,27,{"c":111,"a":178,"b":"yellow","d":25},"red","blue"]},[-3,5,["orange","blue"],70],"yellow"],"a":[{"e":{"c":-15,"a":"red","b":-18,"d":"green"},"a":-42,"d":{"c":-47,"a":"red","b":"green","d":"yellow"},"c":{"e":56,"a":"green","d":"yellow","c":"orange","h":"yellow","b":"blue","g":-35,"f":179,"i":"green"},"h":"blue","b":[35,153],"g":193,"f":{"e":{"e":37,"c":86,"a":"green","g":170,"b":"violet","d":"red","f":-33},"c":187,"a":16,"b":147,"d":19,"f":"red"},"i":88},"green","blue",{"e":{"e":"orange","a":"blue","d":"orange","c":150,"h":-12,"b":"green","g":"red","f":145,"i":"red"},"c":"red","a":"yellow","b":"yellow","d":"blue"},196,{"e":"green","c":186,"a":"green","g":-18,"b":"red","d":[102,"green","orange",[-6,160,128,"green","violet",48,"violet","yellow",50],"blue","green","orange",[199,59,20,15,126]],"f":[[120,"red",69],49,18,84,"red","green",["orange","blue",-31,"green","red",198,115]]},{"e":"yellow","a":196,"d":["orange",{"e":"violet","a":21,"d":"green","c":"red","h":"green","b":18,"g":48,"f":174,"i":"orange"},{"a":-1,"b":"green"},"green"],"c":106,"h":"blue","b":"blue","g":"yellow","f":{"e":-4,"a":61,"d":18,"c":122,"h":"green","b":84,"g":165,"f":"orange"}}],"b":[43,{"e":137,"c":"green","a":"green","b":75,"d":125}],"d":{"e":178,"c":[-21,[116,20,"yellow","blue",161,"orange","blue",30,{"c":181,"a":-30,"b":3}],"orange",-9,"orange",["violet","green",54],"orange",[-20,97,{"c":59,"a":115,"b":-48,"d":-22},28,{"e":59,"c":"green","a":"green","b":"yellow","d":"green","f":-27}],{"a":"violet"},{"e":"blue","c":50,"a":"orange","b":"yellow","d":"orange","f":{"a":"red","b":"green"}}],"a":"orange","b":134,"d":-3,"f":{"e":"violet","a":"orange","d":"green","c":80,"h":"red","b":[140],"g":"red","f":"red"}}},-47,[-28,{"a":[46,["blue",-45,172,193,"blue","green",-2],122,{"a":"green","b":92},-35,[136,[-8,127,20,91,45,"orange"],"green","orange",["orange","yellow",92,162,48,"orange","violet",197],"blue","orange",57,172],"green",135],"b":{"e":[173,{"e":89,"a":96,"d":"orange","c":"orange","h":"green","b":74,"g":"yellow","f":60,"i":135},-11,3,"blue","violet","blue"],"c":160,"a":"blue","b":60,"d":"green","f":"red"}},{"e":[12,"orange"],"c":{"e":45,"c":{"e":-26,"a":86,"d":"yellow","c":["yellow",128,180,135,102,186,"red",194,"green"],"h":"violet","b":{"c":90,"a":-47,"b":56},"g":"blue","f":"red","i":28},"a":71,"b":"violet","d":25},"a":182,"b":"green","d":111,"f":"violet"},{"c":[-21,{"e":[157,13,"red",180,"yellow","green","red",59],"c":-49,"a":82,"b":69,"d":{"e":"orange","a":"yellow","d":98,"j":60,"c":"red","h":199,"b":172,"g":120,"f":"yellow","i":98}},[53,[-23,"orange",135,102,165,170,172,"violet"],"yellow","blue","green",105,97],[74,"violet","orange",["yellow",56,"orange",81,"violet"],"orange",177,75,11],"blue","yellow","blue","red",["blue"]],"a":{"a":-36,"b":"orange"},"b":5,"d":"green"},179,-26,{"a":"green","b":[163,{"a":"orange","b":-35},{"e":180,"a":"blue","d":40,"j":"orange","c":"green","h":"orange","b":"orange","g":131,"f":53,"i":169}]}]],[[{"e":153,"c":"orange","a":"yellow","g":{"e":-4,"c":{"e":115,"c":"red","a":121,"b":151,"d":"red"},"a":"orange","b":194,"d":"orange"},"b":{"a":30},"d":["green",[31,["violet"],["orange",152,"yellow","red"],"yellow"],"violet",{"a":"violet"},171,"violet",{"c":"violet","a":"orange","b":"orange"},"green"],"f":{"e":"yellow","c":["green","red"],"a":"red","g":-17,"b":-3,"d":-42,"f":150}},"orange",{"c":["orange"],"a":"red","b":{"c":-32,"a":{"a":"blue","b":"orange"},"b":{"c":75,"a":{"e":"green","a":101,"d":-3,"j":"violet","c":56,"h":166,"b":192,"g":-5,"f":-22,"i":100},"b":"yellow"},"d":159},"d":{"e":"violet","a":"yellow","d":["green","orange","blue","green"],"j":"yellow","c":[23,"green","blue","yellow","violet","red"],"h":[149,-24,"red",152],"b":-12,"g":"red","f":89,"i":169}},[{"e":{"e":"violet","a":[124,"blue","orange","green",160],"d":113,"c":"red","h":"blue","b":["violet","red","violet",104],"g":85,"f":179,"i":{"e":"orange","a":"violet","d":"violet","j":"yellow","c":191,"h":"red","b":53,"g":-25,"f":"green","i":169}},"a":[147,120],"d":"green","j":["yellow",["yellow",108,"violet",114,"green",195,25,"green"],"green","orange"],"c":["orange",148,141,"yellow",32,-24],"h":124,"b":"orange","g":[121,"blue","red","violet",-18],"f":"violet","i":"red"},["violet","green",94,91,"blue"],{"c":[9,"violet",-18,69,"orange","orange",-24,"yellow","yellow"],"a":"yellow","b":150,"d":73}],110,["blue",-9,"blue","yellow",{"e":[163,45,67],"c":135,"a":50,"b":[43,26,18,120,"green","blue",10,"green",68,-2],"d":{"e":136,"a":15,"d":89,"c":[65,"green",108,122,"yellow","violet","yellow"],"h":"green","b":{"e":125,"a":21,"d":51,"c":153,"h":33,"b":158,"g":"blue","f":26,"i":"green"},"g":-33,"f":[-16,14,"red","red",126,"violet",-16]}},"red",{"e":120,"c":"red","a":{"e":"green","a":"yellow","d":18,"c":150,"h":185,"b":["yellow","red","violet",48,"violet","blue"],"g":"green","f":{"a":"yellow"}},"g":[12,"blue",168,"orange",{"a":"red"},168,"red"],"b":-2,"d":{"e":"violet","c":"yellow","a":86,"g":155,"b":3,"d":-24,"f":149},"f":"yellow"}],[{"c":"green","a":91,"b":"green"},[{"e":"red","c":98,"a":{"c":"red","a":-49,"b":176,"d":105},"b":"red","d":169,"f":"blue"},-46,"green","blue",-30,{"c":154,"a":72,"b":"yellow"},[161,85]],111,"blue",172,57,{"a":-23,"b":"green"},81,[160,["blue","green","green",157],137,["blue",["violet",162],[153,"yellow","orange","violet",127,"green",148,182,29,150],-33,168,"blue"],95]]],["yellow",132,{"c":16,"a":[113],"b":"red","d":{"e":"green","c":24,"a":{"a":"red"},"g":125,"b":["violet",2,101],"d":"green","f":132}},89],[{"e":"yellow","c":[68,"green",[160,146],175,"orange",185,"blue",[198,[179,"yellow","green",72,33,112,179,"violet",194,1]]],"a":["blue","violet",153,"blue",{"a":77,"b":"yellow"},-19,"yellow","green"],"b":"yellow","d":29,"f":{"c":"violet","a":"violet","b":-18}},"yellow",{"e":["violet","green",["red","red","blue",126,137,47,["blue","blue","green",102,"orange","yellow","green"],"red"],[["blue","orange",16,-2],"green","orange","yellow",27,150,0],{"e":62,"c":"yellow","a":"blue","b":94,"d":10,"f":31},[-47,{"e":"green","a":"yellow","d":197,"j":99,"c":"yellow","h":152,"b":"orange","g":85,"f":"green","i":"green"},174,"blue","green"],106],"c":["blue",118,[120,161,-41,["violet"],"violet","yellow"],"red"],"a":["orange","green","red",58,"green",5,178,191,-43],"b":62,"d":"violet"},{"e":97,"a":[["yellow",122,"orange","red",{"e":165,"c":"blue","a":63,"g":"violet","b":"blue","d":"yellow","f":77},[104,118,"green","red","orange",61]],{"c":124,"a":86,"b":"violet"},"yellow",{"e":[98,110,40,104,126,90,140,"blue",46],"a":-42,"d":[77,"green","red",-28,"blue",88,29,-9,-28],"j":-3,"c":"green","h":{"e":153,"c":45,"a":"green","g":127,"b":"red","d":183,"f":"orange"},"b":3,"g":0,"f":["blue","green",62,"red","yellow","green"],"i":"orange"},127,171,[118,[119,"blue"],15,87,"orange",{"e":-34,"c":"violet","a":18,"b":153,"d":37,"f":"red"}],"red"],"d":"red","c":["yellow","red","blue","red","violet","red",-13,179],"h":100,"b":{"e":"orange","a":48,"d":"red","c":{"e":"violet","a":"violet","d":"orange","c":["violet","yellow","blue","yellow"],"h":"red","b":"violet","g":149,"f":"green"},"h":"yellow","b":46,"g":"blue","f":184},"g":16,"f":36,"i":{"e":"green","a":-4,"d":{"c":{"e":"yellow","a":"blue","d":177,"j":"violet","c":"red","h":140,"b":131,"g":137,"f":53,"i":28},"a":16,"b":161},"c":48,"h":[53,7,[137,80,113,160,"blue",105]],"b":"blue","g":"orange","f":"green"}},"yellow",{"c":["orange",{"e":-11,"c":-36,"a":"green","b":"yellow","d":"yellow","f":{"a":186}},-4,170,"green","green",16,123],"a":-29,"b":{"c":"orange","a":"blue","b":"orange"}},{"c":[[161,"violet","blue"],"yellow","yellow",["red",22,["red",92,103,126,-13,67,"blue"],-21,136,"violet",[193]],"blue",-15],"a":{"e":179,"a":1,"d":"yellow","c":[92,15],"h":"orange","b":{"e":-6,"a":"violet","d":"yellow","j":155,"c":198,"h":-18,"b":14,"g":"blue","f":-39,"i":"orange"},"g":{"a":"yellow"},"f":"blue"},"b":87},"yellow",{"c":"blue","a":[{"e":"red","a":"blue","d":"orange","c":"orange","h":-27,"b":"yellow","g":47,"f":{"e":"violet","a":"green","d":185,"j":"orange","c":"violet","h":138,"b":-3,"g":"blue","f":"red","i":150}},{"e":75,"c":168,"a":[12,"blue","green"],"g":{"c":"blue","a":"green","b":107},"b":-36,"d":"orange","f":72},2,[120,"green",182,"yellow",-23,"red"],"green",{"e":"blue","a":"orange","d":"blue","c":157,"h":"green","b":58,"g":"blue","f":-39},"red","orange",32],"b":"violet"},{"e":178,"a":{"c":{"e":59,"a":186,"d":"orange","c":{"a":"violet"},"h":"green","b":198,"g":{"a":"blue","b":"blue"},"f":"orange","i":2},"a":"red","b":[95,130,"blue","violet",98]},"d":176,"c":-38,"h":["yellow",128,"green",39,74,"yellow",5],"b":"blue","g":"violet","f":"orange"}],{"a":{"e":135,"c":{"e":{"e":["orange",-44,81,-11,-1,47,"orange",-36],"c":10,"a":12,"b":"red","d":{"e":"violet","c":"violet","a":161,"b":192,"d":133},"f":77},"c":92,"a":"yellow","g":["blue"],"b":{"e":"violet","a":-48,"d":"orange","c":"blue","h":"orange","b":-40,"g":81,"f":77},"d":102,"f":"yellow"},"a":127,"b":"violet","d":{"e":130,"a":36,"d":148,"c":"yellow","h":117,"b":"orange","g":"orange","f":-19,"i":["green",{"e":"red","a":191,"d":159,"j":"violet","c":"red","h":147,"b":"blue","g":"red","f":"red","i":"green"},"blue",62]},"f":-5},"b":[["green",-36,62,"green","blue",{"c":"violet","a":{"a":-4},"b":"violet","d":88},{"e":"yellow","c":166,"a":["blue"],"g":50,"b":146,"d":"blue","f":142},"yellow"]]},["red","blue",{"e":{"a":-16},"a":[["violet",{"e":"blue","a":171,"d":"blue","c":"blue","h":"green","b":"green","g":"orange","f":"yellow"},186,"orange",195,87,"green",[67,158,"blue",23]],25],"d":[159,74],"c":-28,"h":{"e":-16,"a":"red","d":55,"c":158,"h":167,"b":"red","g":27,"f":{"e":"yellow","c":[34,"blue",-22,"orange"],"a":94,"b":-30,"d":["blue",133,39,102,"orange"]}},"b":119,"g":{"e":104,"c":90,"a":["orange","blue",158,-34,"violet"],"g":"green","b":33,"d":["violet",125,"yellow","yellow",117,["blue",25,"orange"],["red",193,-23,"red","green",146,173],"red","yellow",10],"f":50},"f":47,"i":{"e":[121,144,172,171,{"e":"green","a":9,"d":"violet","c":-33,"h":64,"b":-4,"g":45,"f":75}],"a":8,"d":{"c":68,"a":["yellow","red","green"],"b":"violet","d":9},"c":"blue","h":17,"b":199,"g":115,"f":[["green",108,113,"red",6,"violet","violet","green",57,"green"],"orange",177,"red",34,"blue","red"],"i":-25}},{"e":"blue","a":["orange","yellow",["violet",27,"violet",128,120,{"e":"green","a":"orange","d":"orange","j":"blue","c":"yellow","h":"yellow","b":"yellow","g":139,"f":132,"i":81},"blue","red",53],7,"orange","violet",{"c":"yellow","a":{"a":-11},"b":"orange","d":87},"violet"],"d":[{"a":175},[163,"orange",185],[-30,109,194,119,170,"green","violet","yellow",125,"red"],"blue",{"e":"green","c":152,"a":37,"b":"red","d":["orange"],"f":"orange"},69,"yellow",{"e":{"e":"yellow","c":"violet","a":144,"b":"yellow","d":141,"f":"blue"},"c":"blue","a":"green","g":"yellow","b":178,"d":"yellow","f":-8},"green",["green","orange",-42,"orange"]],"c":"red","h":54,"b":"orange","g":["green",2,146,-6,{"e":52,"a":"orange","d":"red","c":"yellow","h":141,"b":35,"g":{"e":"violet","a":"blue","d":"yellow","c":"blue","h":100,"b":119,"g":"blue","f":"yellow"},"f":136,"i":"yellow"},194,["yellow",139,"green",["blue",14,"green","blue","blue",119,"violet",-5],{"a":61,"b":"orange"},"violet"]],"f":20},{"c":"orange","a":{"e":135,"c":"violet","a":111,"g":{"a":"red","b":96},"b":186,"d":33,"f":127},"b":{"e":83,"a":"yellow","d":"orange","c":"blue","h":"orange","b":0,"g":"orange","f":164,"i":"blue"},"d":-28},-42,{"e":"green","c":{"e":-20,"c":"yellow","a":66,"b":156,"d":"violet"},"a":[-9,129],"g":74,"b":{"e":"violet","c":"green","a":[52,"blue",["green",-8,"green","green"],"red",188,43,"green",{"e":"orange","a":40,"d":-6,"c":"orange","h":93,"b":"green","g":103,"f":"red"}],"g":{"e":"red","c":"yellow","a":16,"b":7,"d":70},"b":{"e":133,"a":150,"d":{"a":34,"b":"green"},"j":166,"c":156,"h":79,"b":"red","g":178,"f":-37,"i":163},"d":"blue","f":"green"},"d":{"c":"blue","a":"violet","b":177,"d":80},"f":[61,88,"yellow",{"c":"blue","a":"orange","b":"violet","d":"yellow"},{"c":119,"a":"violet","b":{"a":"red"},"d":84},95,170]},{"e":191,"c":2,"a":"orange","b":{"a":[-9,"green","violet",["green",132,"red",61,85],3,2]},"d":["green","yellow","violet",-46,48,"green"]}],["violet",["yellow"],["blue",{"e":-15,"c":{"e":["green","violet",0,3,183,165,-1,"orange","blue"],"a":"violet","d":20,"c":"violet","h":"yellow","b":60,"g":"violet","f":163,"i":135},"a":71,"b":{"c":[115,"green",25,"yellow","blue",66],"a":"yellow","b":"green"},"d":"violet","f":{"e":"yellow","c":"blue","a":"blue","b":59,"d":[69,71,"yellow","red",99,"green","yellow",144,43,-38]}},"yellow","blue","orange",55,{"c":[-9,-16,"green",100,28,"red","blue","blue",174],"a":-31,"b":106,"d":"violet"},"blue"],[141,[32,"orange",{"c":-6,"a":-7,"b":64},["blue",{"e":-24,"c":"yellow","a":153,"b":"orange","d":"blue","f":"violet"},"violet",-28,197,"yellow","green","green"],2,{"e":"violet","c":"red","a":"red","b":"yellow","d":"yellow"},["yellow"],"red"],["green",[-16,47],"blue",87,"red","green"],105,"violet",[[127,"violet",81],"red","blue",[193,178,-6],20,"red",61],-2,"blue",-35],{"c":-12,"a":189,"b":"red"},[12,196,["red",27,["violet","green",15,["yellow","green",152,56,187,"yellow",69],"violet"]],127,{"a":"yellow"},-35,[[180,{"c":"yellow","a":191,"b":"violet","d":"red"},41,33,-5],188,"red","violet",23,100,30,91,-15],"blue"],[[129,"blue",{"a":"red"},"violet","green",56,["yellow"],180,[156,"violet"],-49],48,"red",38,{"a":{"e":93,"a":"yellow","d":170,"c":{"e":116,"a":5,"d":89,"j":"blue","c":"blue","h":"red","b":"blue","g":-2,"f":"red","i":-7},"h":148,"b":149,"g":"red","f":-18,"i":-44}},101,183],[25,16,123]],{"e":{"c":"orange","a":{"e":[{"c":"green","a":"orange","b":100,"d":-30}],"a":187,"d":"green","c":{"a":122},"h":-10,"b":118,"g":-12,"f":63},"b":{"e":["violet","yellow","yellow",167,163,5],"a":-28,"d":[-2,61,"red",-18,"red",{"e":"yellow","a":"orange","d":"yellow","j":"green","c":"orange","h":-10,"b":-32,"g":115,"f":141,"i":164},"red",["violet",99,"orange","blue","orange","green","green","violet","yellow"]],"c":-24,"h":"blue","b":"violet","g":47,"f":156},"d":195},"a":{"a":[176]},"d":["violet",[["green",180,"violet","yellow"],{"e":133,"a":"violet","d":{"e":57,"a":"yellow","d":57,"j":"violet","c":"red","h":33,"b":"green","g":"yellow","f":"green","i":79},"j":"orange","c":"violet","h":62,"b":"blue","g":-37,"f":"violet","i":93},-43,"violet",103,"yellow",194,56],{"e":-19,"c":"yellow","a":"orange","b":-19,"d":"red","f":"yellow"}],"c":162,"h":[73,"green",[87,{"a":"green"}],[56,"green",[["green",-2,"green",-47,"yellow",-39,47],"red",129,[90,181,50,"green","green","green","blue",7,"violet"],-3,9,-12,171],"red","orange",159,["violet","yellow",77,86,"yellow","yellow","red",185],145,[81,133]],"yellow",-3,[{"c":["orange",108,82],"a":"violet","b":{"a":"yellow","b":"yellow"},"d":42}],"orange",[-27,["green",{"c":"violet","a":"violet","b":"orange","d":15},78],"red",23],"orange"],"b":{"e":"yellow","a":46,"d":[118,31,142,{"e":-48,"a":"blue","d":"green","c":"violet","h":69,"b":"orange","g":178,"f":"orange","i":"green"},{"e":109,"a":"orange","d":-7,"c":42,"h":168,"b":"blue","g":157,"f":{"a":93,"b":142},"i":38},[59,80],"orange",73,"violet"],"c":122,"h":{"e":153,"c":"yellow","a":11,"b":"orange","d":101},"b":"blue","g":"orange","f":[{"e":48,"c":-39,"a":77,"g":-33,"b":"yellow","d":30,"f":36},153,{"c":"violet","a":78,"b":63,"d":"orange"},117],"i":"red"},"g":"yellow","f":{"a":"orange","b":[{"a":87},"violet","orange",96,154,"orange","violet",{"a":[-45],"b":103}]}},"red"],[{"e":"violet","a":{"e":167,"a":{"e":"orange","c":{"e":"red","c":76,"a":"green","b":"violet","d":146,"f":152},"a":"violet","b":-8,"d":76,"f":"red"},"d":[102,"yellow","blue","blue",22,73],"c":"red","h":{"e":{"c":92,"a":178,"b":{"e":-43,"a":"yellow","d":136,"j":"red","c":193,"h":98,"b":"orange","g":49,"f":"yellow","i":"violet"}},"a":54,"d":138,"j":[0,[177,178],"red",52,[87,"violet",123,"orange","orange","yellow",48,"yellow"],"violet",100,"blue"],"c":3,"h":"green","b":175,"g":{"e":[175,-25,-47,"orange",60,185],"a":["orange",-49,156],"d":"yellow","j":8,"c":-28,"h":129,"b":[89,-12,67,"green",195,"red","violet",150,"red",106],"g":"violet","f":-29,"i":123},"f":"orange","i":71},"b":[172,["yellow","violet","green","blue",194],-46,{"a":102,"b":"green"}],"g":[{"e":{"c":23,"a":"yellow","b":-25},"a":"blue","d":"green","j":185,"c":"yellow","h":["orange","violet",-21],"b":{"c":191,"a":197,"b":"yellow"},"g":115,"f":-41,"i":"blue"},-17,[-23,64,"red",8,"orange",[105,-11,29,-23,30,65,15],170],"yellow","yellow",-46,"green","orange",143],"f":177},"d":"red","c":"red","h":135,"b":{"e":"red","a":"orange","d":["violet",[23,"red","violet","orange",66,{"c":"orange","a":"green","b":169,"d":57},"blue",125,"green"],110,135,[-40,"violet","yellow",-26,-23,44],"orange",28],"c":"orange","h":107,"b":91,"g":105,"f":{"e":{"e":164,"c":180,"a":"blue","b":"yellow","d":144},"c":"violet","a":"violet","g":95,"b":"red","d":"violet","f":"green"},"i":156},"g":["violet",108,["blue","yellow","red",[23,"yellow",3,159,112],{"e":-41,"c":"green","a":22,"b":"violet","d":"blue","f":"blue"},"violet",-27,"green","violet",-17],"green",{"e":{"e":[-19,96,-28,"orange"],"c":"yellow","a":"yellow","g":124,"b":97,"d":{"a":"blue"},"f":"green"},"a":46,"d":["blue",118,"yellow","yellow","yellow","green",["yellow",-10,90,167,"red",54,-15]],"j":106,"c":"red","h":[{"e":"red","c":28,"a":"yellow","b":170,"d":"blue","f":105},-40,"orange",188,"yellow",142],"b":117,"g":"violet","f":{"a":[-5,"red",46,182,"red","orange"]},"i":"yellow"},["orange",88,18,{"e":"blue","a":"violet","d":"blue","c":"violet","h":"violet","b":196,"g":103,"f":67,"i":13}],"blue","blue"],"f":-19,"i":{"a":166}},164],[{"e":{"e":"blue","a":13,"d":{"e":{"e":"orange","a":88,"d":"red","c":"yellow","h":[93,79],"b":"orange","g":109,"f":34,"i":-13},"a":-44,"d":"red","j":[173,78,"red",{"e":"yellow","a":-32,"d":"blue","j":"violet","c":"blue","h":119,"b":"green","g":-30,"f":193,"i":95},"orange",-43,-16],"c":["green",{"e":41,"c":"red","a":109,"b":159,"d":59},173,18,"violet",21,"red"],"h":"blue","b":44,"g":{"a":129,"b":-10},"f":-26,"i":27},"j":"red","c":{"a":{"c":-28,"a":"green","b":188,"d":"blue"},"b":"blue"},"h":[183,[118,[-7,"orange"],132,[23,175,"yellow","green",11,178,171,"orange","blue",18],134,1,"green",[-9,99],103,-25],"red",[65,"red","blue"],"violet","blue"],"b":"yellow","g":164,"f":-9,"i":{"c":51,"a":"green","b":115,"d":{"a":27,"b":"red"}}},"a":[{"e":[143,"violet",128,"red","yellow",185,"green","red","red"],"a":29,"d":"red","c":170,"h":[131,"violet",96,{"a":"yellow","b":"green"},139,22,176,"yellow",[-46,-14,"red","blue",83,141],[132,108,"blue","blue","green",197,"yellow"]],"b":"blue","g":"orange","f":"yellow","i":[-9]},"violet",56,[169,12,155,["red",197,{"e":"violet","a":22,"d":"violet","c":84,"h":"red","b":70,"g":"violet","f":-41},47,"violet"],[["green","green",179,56,"green","violet",171,"violet","violet"],"blue","red","green",-17,"green",190],"green","red",146,60],"yellow","red","yellow","violet"],"d":[[141,40,"yellow",1,"blue","green","yellow",{"e":13,"a":"blue","d":"red","c":"red","h":176,"b":"violet","g":164,"f":4,"i":"violet"}]],"c":72,"h":15,"b":"yellow","g":{"e":[-12,"blue",["red","blue",11],29,{"e":59,"c":"red","a":{"e":55,"a":"blue","d":"orange","c":"yellow","h":"violet","b":-19,"g":"green","f":"violet","i":197},"b":"orange","d":"violet","f":90},[-14,154,"violet","orange",74,{"e":"yellow","a":"violet","d":66,"c":"yellow","h":80,"b":"yellow","g":"yellow","f":"orange","i":"blue"},"green","red",116,149],"green",108],"c":{"c":28,"a":"blue","b":"yellow","d":"blue"},"a":36,"b":["orange","green","orange","green","red",46,55,"blue",["violet",98,[163,-35,163,-28],"blue","red",155,"blue"],-8],"d":163},"f":[9,"green",{"c":"green","a":"violet","b":68,"d":"yellow"},114,33,1,-25]},["red",[-20,{"c":"yellow","a":"red","b":"green","d":{"a":"red"}},"red",[[141,76],[174],100,{"e":126,"c":39,"a":["violet",94,"orange",102,"blue"],"b":55,"d":"yellow","f":"yellow"},146,{"c":169,"a":"red","b":"red"},[["green",-48,"violet","orange"],[80,-7,-22,"yellow","orange","yellow",185,"orange"],"green","violet","orange"],"red"],"yellow"],[{"a":["orange","blue"],"b":[{"a":-42,"b":"violet"},"green",99,-20,"blue"]},{"c":"blue","a":"violet","b":14,"d":9},"green",{"c":["blue",148,[38,139,125,52,"red",40,190,"yellow",21,"violet"],"violet",110,"green"],"a":{"c":97,"a":[35,"orange",44,"red",87,"orange","blue",61,"yellow"],"b":176,"d":144},"b":137},85,[192,-37,"orange",{"c":"yellow","a":-10,"b":[71]},"yellow",176,["green",14],{"a":102},-39],"violet",164],-9,"blue",[["blue"],70]]]]
"""

    let day13input =
        """
Alice would lose 2 happiness units by sitting next to Bob.
Alice would lose 62 happiness units by sitting next to Carol.
Alice would gain 65 happiness units by sitting next to David.
Alice would gain 21 happiness units by sitting next to Eric.
Alice would lose 81 happiness units by sitting next to Frank.
Alice would lose 4 happiness units by sitting next to George.
Alice would lose 80 happiness units by sitting next to Mallory.
Bob would gain 93 happiness units by sitting next to Alice.
Bob would gain 19 happiness units by sitting next to Carol.
Bob would gain 5 happiness units by sitting next to David.
Bob would gain 49 happiness units by sitting next to Eric.
Bob would gain 68 happiness units by sitting next to Frank.
Bob would gain 23 happiness units by sitting next to George.
Bob would gain 29 happiness units by sitting next to Mallory.
Carol would lose 54 happiness units by sitting next to Alice.
Carol would lose 70 happiness units by sitting next to Bob.
Carol would lose 37 happiness units by sitting next to David.
Carol would lose 46 happiness units by sitting next to Eric.
Carol would gain 33 happiness units by sitting next to Frank.
Carol would lose 35 happiness units by sitting next to George.
Carol would gain 10 happiness units by sitting next to Mallory.
David would gain 43 happiness units by sitting next to Alice.
David would lose 96 happiness units by sitting next to Bob.
David would lose 53 happiness units by sitting next to Carol.
David would lose 30 happiness units by sitting next to Eric.
David would lose 12 happiness units by sitting next to Frank.
David would gain 75 happiness units by sitting next to George.
David would lose 20 happiness units by sitting next to Mallory.
Eric would gain 8 happiness units by sitting next to Alice.
Eric would lose 89 happiness units by sitting next to Bob.
Eric would lose 69 happiness units by sitting next to Carol.
Eric would lose 34 happiness units by sitting next to David.
Eric would gain 95 happiness units by sitting next to Frank.
Eric would gain 34 happiness units by sitting next to George.
Eric would lose 99 happiness units by sitting next to Mallory.
Frank would lose 97 happiness units by sitting next to Alice.
Frank would gain 6 happiness units by sitting next to Bob.
Frank would lose 9 happiness units by sitting next to Carol.
Frank would gain 56 happiness units by sitting next to David.
Frank would lose 17 happiness units by sitting next to Eric.
Frank would gain 18 happiness units by sitting next to George.
Frank would lose 56 happiness units by sitting next to Mallory.
George would gain 45 happiness units by sitting next to Alice.
George would gain 76 happiness units by sitting next to Bob.
George would gain 63 happiness units by sitting next to Carol.
George would gain 54 happiness units by sitting next to David.
George would gain 54 happiness units by sitting next to Eric.
George would gain 30 happiness units by sitting next to Frank.
George would gain 7 happiness units by sitting next to Mallory.
Mallory would gain 31 happiness units by sitting next to Alice.
Mallory would lose 32 happiness units by sitting next to Bob.
Mallory would gain 95 happiness units by sitting next to Carol.
Mallory would gain 91 happiness units by sitting next to David.
Mallory would lose 66 happiness units by sitting next to Eric.
Mallory would lose 75 happiness units by sitting next to Frank.
Mallory would lose 99 happiness units by sitting next to George.
"""

    let day16input = """
Sue 1: cars: 9, akitas: 3, goldfish: 0
Sue 2: akitas: 9, children: 3, samoyeds: 9
Sue 3: trees: 6, cars: 6, children: 4
Sue 4: trees: 4, vizslas: 4, goldfish: 9
Sue 5: akitas: 9, vizslas: 7, cars: 5
Sue 6: vizslas: 6, goldfish: 6, akitas: 3
Sue 7: pomeranians: 5, samoyeds: 0, perfumes: 10
Sue 8: cars: 10, pomeranians: 7, goldfish: 8
Sue 9: trees: 2, vizslas: 7, samoyeds: 6
Sue 10: perfumes: 5, pomeranians: 4, children: 9
Sue 11: vizslas: 5, perfumes: 8, cars: 10
Sue 12: children: 10, cars: 6, perfumes: 5
Sue 13: cats: 4, samoyeds: 7, pomeranians: 8
Sue 14: perfumes: 6, goldfish: 10, children: 7
Sue 15: perfumes: 4, pomeranians: 3, cars: 6
Sue 16: perfumes: 7, cars: 9, pomeranians: 6
Sue 17: goldfish: 3, cars: 6, vizslas: 7
Sue 18: perfumes: 6, cars: 7, goldfish: 3
Sue 19: trees: 0, akitas: 3, pomeranians: 8
Sue 20: goldfish: 6, trees: 2, akitas: 6
Sue 21: pomeranians: 9, akitas: 9, samoyeds: 9
Sue 22: vizslas: 2, cars: 9, perfumes: 5
Sue 23: goldfish: 10, samoyeds: 8, children: 9
Sue 24: akitas: 4, goldfish: 1, vizslas: 5
Sue 25: goldfish: 10, trees: 8, perfumes: 6
Sue 26: vizslas: 5, akitas: 8, trees: 1
Sue 27: trees: 3, cars: 6, perfumes: 2
Sue 28: goldfish: 8, trees: 7, akitas: 10
Sue 29: children: 5, trees: 1, goldfish: 10
Sue 30: vizslas: 3, perfumes: 8, akitas: 3
Sue 31: cars: 6, children: 10, perfumes: 7
Sue 32: cars: 10, perfumes: 3, goldfish: 10
Sue 33: perfumes: 9, vizslas: 3, akitas: 4
Sue 34: perfumes: 10, vizslas: 7, children: 8
Sue 35: cars: 5, perfumes: 5, vizslas: 9
Sue 36: trees: 9, cars: 9, akitas: 7
Sue 37: samoyeds: 9, perfumes: 2, cars: 10
Sue 38: akitas: 7, cars: 5, trees: 5
Sue 39: goldfish: 8, trees: 9, cars: 10
Sue 40: trees: 0, cats: 1, pomeranians: 1
Sue 41: pomeranians: 6, perfumes: 9, samoyeds: 1
Sue 42: vizslas: 6, akitas: 3, pomeranians: 1
Sue 43: vizslas: 2, perfumes: 3, pomeranians: 6
Sue 44: akitas: 5, pomeranians: 0, vizslas: 10
Sue 45: vizslas: 4, goldfish: 1, cars: 5
Sue 46: cars: 4, vizslas: 8, cats: 0
Sue 47: cats: 5, children: 8, pomeranians: 2
Sue 48: vizslas: 3, perfumes: 6, cats: 0
Sue 49: akitas: 7, perfumes: 0, trees: 7
Sue 50: trees: 4, akitas: 10, vizslas: 2
Sue 51: goldfish: 10, cars: 9, trees: 4
Sue 52: cars: 5, children: 9, perfumes: 0
Sue 53: vizslas: 5, cars: 3, cats: 8
Sue 54: cars: 5, akitas: 1, goldfish: 10
Sue 55: akitas: 10, vizslas: 2, cars: 6
Sue 56: cats: 6, trees: 0, cars: 4
Sue 57: vizslas: 1, akitas: 1, samoyeds: 7
Sue 58: samoyeds: 6, vizslas: 1, akitas: 7
Sue 59: akitas: 9, cars: 8, vizslas: 1
Sue 60: cars: 6, vizslas: 7, goldfish: 0
Sue 61: pomeranians: 5, akitas: 6, vizslas: 2
Sue 62: samoyeds: 2, cats: 8, goldfish: 7
Sue 63: vizslas: 10, goldfish: 7, samoyeds: 9
Sue 64: perfumes: 2, trees: 1, akitas: 6
Sue 65: cars: 8, perfumes: 10, vizslas: 9
Sue 66: akitas: 8, vizslas: 8, perfumes: 8
Sue 67: goldfish: 7, cars: 9, samoyeds: 9
Sue 68: perfumes: 2, children: 7, akitas: 1
Sue 69: perfumes: 7, vizslas: 9, akitas: 1
Sue 70: samoyeds: 3, vizslas: 1, trees: 1
Sue 71: vizslas: 8, goldfish: 7, trees: 9
Sue 72: goldfish: 8, cars: 6, trees: 9
Sue 73: perfumes: 5, cars: 10, samoyeds: 7
Sue 74: pomeranians: 4, perfumes: 3, cars: 5
Sue 75: samoyeds: 1, perfumes: 1, pomeranians: 1
Sue 76: goldfish: 4, cats: 6, akitas: 7
Sue 77: perfumes: 5, akitas: 4, vizslas: 8
Sue 78: perfumes: 4, cats: 3, children: 4
Sue 79: vizslas: 5, pomeranians: 9, samoyeds: 7
Sue 80: cars: 3, samoyeds: 5, pomeranians: 7
Sue 81: vizslas: 2, samoyeds: 4, perfumes: 2
Sue 82: trees: 1, akitas: 10, vizslas: 9
Sue 83: vizslas: 0, akitas: 2, samoyeds: 5
Sue 84: perfumes: 5, vizslas: 7, children: 8
Sue 85: cats: 3, children: 2, trees: 0
Sue 86: cars: 3, perfumes: 2, goldfish: 2
Sue 87: trees: 1, akitas: 7, vizslas: 0
Sue 88: trees: 1, akitas: 2, samoyeds: 1
Sue 89: cars: 4, vizslas: 8, akitas: 1
Sue 90: perfumes: 5, cats: 3, vizslas: 0
Sue 91: samoyeds: 7, cats: 6, goldfish: 8
Sue 92: samoyeds: 10, cats: 0, cars: 7
Sue 93: cars: 6, akitas: 7, samoyeds: 2
Sue 94: perfumes: 0, goldfish: 6, trees: 9
Sue 95: cars: 6, pomeranians: 2, samoyeds: 8
Sue 96: cars: 2, trees: 9, samoyeds: 4
Sue 97: goldfish: 5, trees: 1, children: 0
Sue 98: akitas: 9, goldfish: 7, children: 6
Sue 99: goldfish: 9, akitas: 0, pomeranians: 0
Sue 100: samoyeds: 6, children: 8, vizslas: 5
Sue 101: vizslas: 6, cars: 5, goldfish: 4
Sue 102: vizslas: 6, akitas: 2, perfumes: 6
Sue 103: samoyeds: 3, akitas: 7, children: 4
Sue 104: cars: 3, perfumes: 10, cats: 6
Sue 105: vizslas: 9, pomeranians: 0, cars: 1
Sue 106: cats: 6, samoyeds: 8, pomeranians: 5
Sue 107: cars: 7, trees: 4, akitas: 10
Sue 108: perfumes: 3, vizslas: 1, goldfish: 9
Sue 109: trees: 6, cars: 8, goldfish: 5
Sue 110: pomeranians: 2, children: 1, vizslas: 7
Sue 111: akitas: 0, vizslas: 8, cars: 0
Sue 112: goldfish: 3, vizslas: 6, akitas: 2
Sue 113: akitas: 10, pomeranians: 7, perfumes: 7
Sue 114: cars: 10, cats: 2, vizslas: 8
Sue 115: akitas: 8, trees: 1, vizslas: 2
Sue 116: vizslas: 2, akitas: 7, perfumes: 1
Sue 117: goldfish: 0, vizslas: 10, trees: 9
Sue 118: trees: 3, cars: 0, goldfish: 0
Sue 119: perfumes: 7, goldfish: 5, trees: 9
Sue 120: children: 9, vizslas: 3, trees: 5
Sue 121: vizslas: 1, goldfish: 7, akitas: 10
Sue 122: perfumes: 1, cars: 6, trees: 1
Sue 123: akitas: 2, vizslas: 0, goldfish: 7
Sue 124: vizslas: 10, pomeranians: 7, akitas: 0
Sue 125: perfumes: 4, cats: 5, vizslas: 2
Sue 126: cars: 6, samoyeds: 8, akitas: 3
Sue 127: trees: 9, goldfish: 7, akitas: 9
Sue 128: cars: 8, trees: 0, perfumes: 2
Sue 129: pomeranians: 7, vizslas: 2, perfumes: 6
Sue 130: vizslas: 9, pomeranians: 3, trees: 6
Sue 131: vizslas: 7, cars: 9, perfumes: 1
Sue 132: akitas: 2, pomeranians: 9, vizslas: 7
Sue 133: trees: 9, pomeranians: 10, samoyeds: 0
Sue 134: children: 4, akitas: 10, perfumes: 4
Sue 135: vizslas: 1, cats: 1, trees: 8
Sue 136: samoyeds: 7, cars: 8, goldfish: 5
Sue 137: perfumes: 0, children: 1, pomeranians: 10
Sue 138: vizslas: 4, perfumes: 5, cars: 5
Sue 139: trees: 2, perfumes: 8, goldfish: 0
Sue 140: cars: 10, akitas: 5, goldfish: 7
Sue 141: children: 4, trees: 3, goldfish: 8
Sue 142: cars: 8, perfumes: 6, trees: 7
Sue 143: akitas: 6, goldfish: 0, trees: 10
Sue 144: akitas: 7, pomeranians: 10, perfumes: 10
Sue 145: trees: 10, vizslas: 3, goldfish: 4
Sue 146: samoyeds: 4, akitas: 3, perfumes: 6
Sue 147: akitas: 8, perfumes: 2, pomeranians: 10
Sue 148: cars: 2, perfumes: 0, goldfish: 8
Sue 149: goldfish: 6, akitas: 7, perfumes: 6
Sue 150: cars: 2, pomeranians: 5, perfumes: 4
Sue 151: goldfish: 1, cars: 5, trees: 0
Sue 152: pomeranians: 4, cars: 7, children: 1
Sue 153: goldfish: 8, cars: 1, children: 10
Sue 154: cars: 6, perfumes: 8, trees: 1
Sue 155: akitas: 4, perfumes: 6, pomeranians: 2
Sue 156: pomeranians: 5, cars: 4, akitas: 1
Sue 157: cats: 5, cars: 9, goldfish: 8
Sue 158: vizslas: 5, samoyeds: 1, children: 7
Sue 159: vizslas: 1, perfumes: 3, akitas: 1
Sue 160: goldfish: 10, pomeranians: 9, perfumes: 5
Sue 161: samoyeds: 3, trees: 7, cars: 2
Sue 162: cars: 2, pomeranians: 1, vizslas: 6
Sue 163: vizslas: 3, perfumes: 5, akitas: 6
Sue 164: vizslas: 1, trees: 0, akitas: 5
Sue 165: vizslas: 5, cars: 6, pomeranians: 8
Sue 166: cars: 10, perfumes: 2, trees: 9
Sue 167: cars: 10, pomeranians: 6, perfumes: 4
Sue 168: akitas: 7, trees: 10, goldfish: 7
Sue 169: akitas: 1, perfumes: 10, cars: 10
Sue 170: akitas: 5, samoyeds: 8, vizslas: 6
Sue 171: children: 3, akitas: 2, vizslas: 3
Sue 172: goldfish: 5, vizslas: 5, perfumes: 9
Sue 173: perfumes: 5, goldfish: 10, trees: 5
Sue 174: akitas: 5, vizslas: 2, children: 7
Sue 175: perfumes: 5, cars: 7, samoyeds: 2
Sue 176: cars: 8, vizslas: 10, akitas: 7
Sue 177: perfumes: 7, children: 8, goldfish: 7
Sue 178: cars: 1, pomeranians: 9, samoyeds: 0
Sue 179: perfumes: 6, cars: 2, trees: 6
Sue 180: trees: 3, vizslas: 7, children: 3
Sue 181: vizslas: 8, samoyeds: 2, trees: 9
Sue 182: perfumes: 3, cats: 1, children: 5
Sue 183: akitas: 9, cats: 6, children: 3
Sue 184: pomeranians: 9, cars: 6, perfumes: 8
Sue 185: vizslas: 9, trees: 0, akitas: 9
Sue 186: perfumes: 6, cars: 5, goldfish: 5
Sue 187: perfumes: 4, cats: 7, vizslas: 2
Sue 188: akitas: 7, cars: 4, children: 10
Sue 189: akitas: 0, goldfish: 7, vizslas: 5
Sue 190: akitas: 5, cars: 5, cats: 6
Sue 191: cars: 6, children: 0, perfumes: 3
Sue 192: cats: 2, perfumes: 10, goldfish: 7
Sue 193: trees: 1, perfumes: 0, cars: 8
Sue 194: perfumes: 9, children: 4, cats: 6
Sue 195: akitas: 7, trees: 3, goldfish: 6
Sue 196: goldfish: 8, cars: 8, samoyeds: 0
Sue 197: cats: 0, akitas: 10, vizslas: 0
Sue 198: goldfish: 1, perfumes: 3, cars: 8
Sue 199: akitas: 10, vizslas: 5, samoyeds: 6
Sue 200: pomeranians: 9, goldfish: 9, samoyeds: 7
Sue 201: samoyeds: 0, goldfish: 7, akitas: 6
Sue 202: vizslas: 0, goldfish: 2, akitas: 1
Sue 203: goldfish: 3, children: 0, vizslas: 8
Sue 204: cars: 8, trees: 2, perfumes: 2
Sue 205: cars: 4, perfumes: 5, goldfish: 8
Sue 206: vizslas: 3, trees: 2, akitas: 1
Sue 207: cars: 7, goldfish: 5, trees: 1
Sue 208: goldfish: 1, cars: 6, vizslas: 8
Sue 209: cats: 4, trees: 1, children: 0
Sue 210: cats: 10, children: 0, perfumes: 0
Sue 211: cars: 4, pomeranians: 7, samoyeds: 5
Sue 212: cars: 2, pomeranians: 10, trees: 1
Sue 213: trees: 10, cats: 5, cars: 10
Sue 214: perfumes: 5, trees: 1, vizslas: 1
Sue 215: akitas: 10, vizslas: 8, samoyeds: 8
Sue 216: vizslas: 2, cats: 5, pomeranians: 3
Sue 217: akitas: 10, perfumes: 0, cats: 10
Sue 218: trees: 8, cats: 5, vizslas: 2
Sue 219: goldfish: 10, perfumes: 8, children: 2
Sue 220: samoyeds: 9, trees: 8, vizslas: 7
Sue 221: children: 7, trees: 6, cars: 6
Sue 222: cats: 4, akitas: 5, pomeranians: 0
Sue 223: trees: 8, goldfish: 2, perfumes: 8
Sue 224: pomeranians: 9, cars: 8, akitas: 5
Sue 225: akitas: 10, vizslas: 0, trees: 2
Sue 226: akitas: 8, cats: 6, cars: 7
Sue 227: trees: 1, akitas: 3, goldfish: 4
Sue 228: pomeranians: 6, cats: 3, goldfish: 3
Sue 229: trees: 10, perfumes: 3, vizslas: 7
Sue 230: perfumes: 8, cars: 7, akitas: 0
Sue 231: perfumes: 10, goldfish: 4, cars: 6
Sue 232: goldfish: 7, trees: 3, cats: 2
Sue 233: perfumes: 6, trees: 4, akitas: 4
Sue 234: goldfish: 9, cats: 4, cars: 7
Sue 235: pomeranians: 6, vizslas: 0, akitas: 6
Sue 236: samoyeds: 5, cars: 5, children: 4
Sue 237: vizslas: 10, cars: 4, goldfish: 4
Sue 238: goldfish: 3, samoyeds: 7, akitas: 2
Sue 239: cats: 8, children: 2, vizslas: 7
Sue 240: cars: 9, perfumes: 4, trees: 9
Sue 241: trees: 8, vizslas: 2, goldfish: 5
Sue 242: cars: 6, trees: 3, vizslas: 3
Sue 243: cats: 6, children: 7, cars: 4
Sue 244: cats: 10, perfumes: 2, goldfish: 7
Sue 245: akitas: 8, cats: 10, perfumes: 8
Sue 246: vizslas: 8, akitas: 5, perfumes: 10
Sue 247: goldfish: 2, vizslas: 5, akitas: 7
Sue 248: akitas: 3, perfumes: 0, trees: 10
Sue 249: cats: 4, vizslas: 5, pomeranians: 6
Sue 250: children: 3, vizslas: 7, perfumes: 2
Sue 251: cars: 0, pomeranians: 10, perfumes: 0
Sue 252: akitas: 0, goldfish: 9, cars: 6
Sue 253: perfumes: 7, cars: 4, samoyeds: 5
Sue 254: akitas: 9, trees: 10, cars: 4
Sue 255: samoyeds: 10, children: 6, akitas: 7
Sue 256: trees: 8, goldfish: 8, perfumes: 8
Sue 257: goldfish: 3, akitas: 2, perfumes: 6
Sue 258: cats: 7, trees: 0, vizslas: 1
Sue 259: perfumes: 7, cars: 7, akitas: 7
Sue 260: goldfish: 0, vizslas: 0, samoyeds: 2
Sue 261: vizslas: 2, children: 2, cats: 3
Sue 262: vizslas: 2, pomeranians: 9, samoyeds: 3
Sue 263: cats: 1, akitas: 3, vizslas: 1
Sue 264: pomeranians: 10, trees: 2, goldfish: 7
Sue 265: samoyeds: 5, trees: 7, perfumes: 4
Sue 266: perfumes: 10, cars: 1, pomeranians: 3
Sue 267: trees: 6, goldfish: 1, cars: 0
Sue 268: cars: 6, samoyeds: 4, pomeranians: 5
Sue 269: goldfish: 3, vizslas: 3, akitas: 3
Sue 270: children: 5, cats: 0, cars: 4
Sue 271: goldfish: 3, perfumes: 8, pomeranians: 7
Sue 272: samoyeds: 6, cars: 7, perfumes: 10
Sue 273: trees: 4, cars: 2, vizslas: 7
Sue 274: samoyeds: 10, perfumes: 9, goldfish: 6
Sue 275: cars: 4, trees: 2, perfumes: 7
Sue 276: akitas: 3, perfumes: 9, cars: 9
Sue 277: akitas: 8, vizslas: 2, cats: 6
Sue 278: trees: 5, goldfish: 7, akitas: 3
Sue 279: perfumes: 9, cars: 8, vizslas: 2
Sue 280: trees: 3, vizslas: 0, children: 0
Sue 281: cars: 7, trees: 2, cats: 5
Sue 282: vizslas: 4, cars: 10, cats: 3
Sue 283: akitas: 10, cats: 3, samoyeds: 9
Sue 284: trees: 7, children: 5, goldfish: 6
Sue 285: cars: 2, perfumes: 5, cats: 7
Sue 286: samoyeds: 5, trees: 10, goldfish: 6
Sue 287: goldfish: 10, perfumes: 4, trees: 7
Sue 288: vizslas: 9, trees: 9, perfumes: 0
Sue 289: trees: 4, goldfish: 9, vizslas: 8
Sue 290: vizslas: 3, cars: 3, trees: 2
Sue 291: goldfish: 2, akitas: 2, trees: 2
Sue 292: children: 1, cars: 0, vizslas: 5
Sue 293: trees: 5, akitas: 4, goldfish: 6
Sue 294: akitas: 3, vizslas: 7, pomeranians: 5
Sue 295: goldfish: 10, vizslas: 3, trees: 1
Sue 296: cars: 2, trees: 1, akitas: 0
Sue 297: akitas: 10, vizslas: 6, samoyeds: 2
Sue 298: children: 5, trees: 1, samoyeds: 9
Sue 299: perfumes: 9, trees: 6, vizslas: 1
Sue 300: akitas: 7, pomeranians: 6, vizslas: 6
Sue 301: cats: 7, children: 6, vizslas: 7
Sue 302: trees: 2, vizslas: 7, samoyeds: 4
Sue 303: goldfish: 0, samoyeds: 10, cars: 4
Sue 304: pomeranians: 9, children: 3, vizslas: 5
Sue 305: akitas: 8, vizslas: 4, cars: 5
Sue 306: akitas: 0, perfumes: 2, pomeranians: 10
Sue 307: akitas: 9, cars: 0, trees: 2
Sue 308: vizslas: 10, goldfish: 8, akitas: 6
Sue 309: trees: 0, cats: 6, perfumes: 2
Sue 310: vizslas: 10, cars: 1, trees: 4
Sue 311: goldfish: 8, perfumes: 6, cats: 3
Sue 312: goldfish: 0, children: 1, akitas: 2
Sue 313: pomeranians: 10, trees: 6, samoyeds: 6
Sue 314: vizslas: 5, akitas: 4, pomeranians: 2
Sue 315: goldfish: 7, trees: 0, akitas: 5
Sue 316: goldfish: 4, vizslas: 5, cars: 7
Sue 317: perfumes: 7, cats: 10, cars: 4
Sue 318: samoyeds: 10, cars: 9, trees: 7
Sue 319: pomeranians: 8, vizslas: 6, cars: 3
Sue 320: cars: 4, cats: 9, akitas: 4
Sue 321: cars: 6, trees: 2, perfumes: 6
Sue 322: goldfish: 1, cats: 2, perfumes: 4
Sue 323: akitas: 6, cats: 5, cars: 8
Sue 324: cats: 4, vizslas: 9, akitas: 0
Sue 325: children: 8, samoyeds: 9, trees: 4
Sue 326: vizslas: 2, samoyeds: 10, perfumes: 7
Sue 327: goldfish: 7, pomeranians: 4, akitas: 10
Sue 328: perfumes: 8, cats: 4, akitas: 10
Sue 329: trees: 0, cars: 9, goldfish: 3
Sue 330: trees: 5, samoyeds: 7, perfumes: 8
Sue 331: cars: 4, perfumes: 2, goldfish: 0
Sue 332: vizslas: 4, pomeranians: 7, akitas: 1
Sue 333: akitas: 4, goldfish: 3, perfumes: 0
Sue 334: samoyeds: 3, akitas: 10, vizslas: 0
Sue 335: goldfish: 1, akitas: 7, vizslas: 6
Sue 336: perfumes: 1, goldfish: 1, pomeranians: 8
Sue 337: children: 5, cars: 4, cats: 4
Sue 338: vizslas: 5, cars: 10, cats: 3
Sue 339: trees: 2, goldfish: 3, cars: 1
Sue 340: trees: 10, goldfish: 6, perfumes: 2
Sue 341: akitas: 5, trees: 6, cats: 3
Sue 342: cars: 10, children: 8, goldfish: 0
Sue 343: cats: 2, akitas: 0, pomeranians: 4
Sue 344: perfumes: 1, vizslas: 3, cars: 3
Sue 345: samoyeds: 8, cats: 5, perfumes: 8
Sue 346: cars: 5, akitas: 10, trees: 2
Sue 347: vizslas: 9, akitas: 9, cars: 3
Sue 348: cars: 3, perfumes: 1, pomeranians: 9
Sue 349: akitas: 1, cars: 4, perfumes: 0
Sue 350: perfumes: 8, vizslas: 2, trees: 6
Sue 351: pomeranians: 5, akitas: 9, cats: 8
Sue 352: pomeranians: 8, vizslas: 3, goldfish: 10
Sue 353: trees: 2, pomeranians: 0, goldfish: 6
Sue 354: cats: 5, akitas: 7, goldfish: 6
Sue 355: goldfish: 6, children: 4, trees: 10
Sue 356: children: 1, trees: 3, akitas: 7
Sue 357: trees: 2, samoyeds: 10, goldfish: 3
Sue 358: samoyeds: 10, cats: 0, goldfish: 0
Sue 359: perfumes: 3, children: 6, pomeranians: 1
Sue 360: cars: 10, pomeranians: 1, samoyeds: 5
Sue 361: samoyeds: 9, pomeranians: 7, perfumes: 6
Sue 362: goldfish: 6, trees: 8, perfumes: 9
Sue 363: samoyeds: 10, pomeranians: 9, children: 10
Sue 364: perfumes: 3, goldfish: 7, cars: 9
Sue 365: cats: 3, children: 4, samoyeds: 8
Sue 366: trees: 0, cars: 10, vizslas: 10
Sue 367: pomeranians: 10, children: 8, perfumes: 2
Sue 368: cars: 5, vizslas: 0, samoyeds: 3
Sue 369: trees: 1, goldfish: 8, cars: 8
Sue 370: vizslas: 0, cars: 2, perfumes: 5
Sue 371: trees: 2, cars: 3, vizslas: 8
Sue 372: trees: 10, children: 9, cats: 1
Sue 373: pomeranians: 3, perfumes: 1, vizslas: 0
Sue 374: vizslas: 0, perfumes: 6, trees: 0
Sue 375: vizslas: 7, pomeranians: 1, akitas: 10
Sue 376: vizslas: 8, trees: 2, cars: 10
Sue 377: perfumes: 9, cats: 5, goldfish: 5
Sue 378: cats: 0, akitas: 10, perfumes: 9
Sue 379: cars: 4, akitas: 1, trees: 1
Sue 380: cars: 4, perfumes: 5, trees: 3
Sue 381: goldfish: 3, akitas: 5, samoyeds: 9
Sue 382: goldfish: 7, perfumes: 5, trees: 5
Sue 383: akitas: 4, cats: 6, cars: 8
Sue 384: children: 6, goldfish: 10, akitas: 7
Sue 385: akitas: 7, vizslas: 5, perfumes: 10
Sue 386: children: 7, vizslas: 10, akitas: 10
Sue 387: goldfish: 6, akitas: 7, trees: 2
Sue 388: vizslas: 6, trees: 1, akitas: 2
Sue 389: cars: 5, vizslas: 3, akitas: 7
Sue 390: vizslas: 4, cats: 8, perfumes: 7
Sue 391: akitas: 3, trees: 0, children: 2
Sue 392: cats: 7, cars: 3, children: 9
Sue 393: trees: 10, vizslas: 3, goldfish: 7
Sue 394: perfumes: 0, goldfish: 7, akitas: 4
Sue 395: cats: 6, cars: 7, vizslas: 0
Sue 396: vizslas: 4, perfumes: 6, goldfish: 5
Sue 397: pomeranians: 8, trees: 1, akitas: 9
Sue 398: goldfish: 7, pomeranians: 6, samoyeds: 9
Sue 399: perfumes: 10, cars: 1, trees: 8
Sue 400: trees: 0, goldfish: 9, children: 6
Sue 401: trees: 1, cars: 6, pomeranians: 8
Sue 402: perfumes: 9, cars: 0, vizslas: 10
Sue 403: samoyeds: 4, akitas: 1, vizslas: 9
Sue 404: perfumes: 0, trees: 2, cars: 4
Sue 405: akitas: 0, perfumes: 5, samoyeds: 4
Sue 406: akitas: 8, vizslas: 6, children: 2
Sue 407: children: 1, trees: 8, goldfish: 10
Sue 408: pomeranians: 4, trees: 10, cars: 9
Sue 409: perfumes: 5, vizslas: 5, akitas: 4
Sue 410: trees: 1, akitas: 10, vizslas: 6
Sue 411: samoyeds: 0, goldfish: 9, perfumes: 7
Sue 412: goldfish: 7, samoyeds: 10, trees: 1
Sue 413: samoyeds: 0, pomeranians: 10, vizslas: 6
Sue 414: children: 2, cars: 10, samoyeds: 2
Sue 415: trees: 2, goldfish: 8, cars: 0
Sue 416: samoyeds: 4, goldfish: 9, trees: 2
Sue 417: trees: 8, akitas: 10, perfumes: 3
Sue 418: samoyeds: 9, goldfish: 2, cars: 1
Sue 419: akitas: 2, perfumes: 8, trees: 2
Sue 420: children: 3, goldfish: 6, perfumes: 5
Sue 421: akitas: 8, perfumes: 2, samoyeds: 6
Sue 422: vizslas: 10, akitas: 4, pomeranians: 3
Sue 423: cats: 8, perfumes: 3, trees: 4
Sue 424: cars: 2, children: 4, pomeranians: 8
Sue 425: pomeranians: 4, samoyeds: 2, goldfish: 4
Sue 426: perfumes: 6, cars: 4, goldfish: 4
Sue 427: akitas: 0, goldfish: 7, perfumes: 5
Sue 428: perfumes: 4, cars: 3, akitas: 5
Sue 429: trees: 0, vizslas: 0, goldfish: 1
Sue 430: perfumes: 4, vizslas: 2, cars: 7
Sue 431: goldfish: 7, pomeranians: 8, trees: 0
Sue 432: goldfish: 7, children: 9, trees: 3
Sue 433: akitas: 1, vizslas: 10, trees: 2
Sue 434: perfumes: 2, cars: 4, goldfish: 10
Sue 435: pomeranians: 6, vizslas: 9, trees: 1
Sue 436: cars: 9, trees: 0, goldfish: 0
Sue 437: trees: 1, goldfish: 1, vizslas: 8
Sue 438: goldfish: 7, samoyeds: 8, children: 2
Sue 439: children: 1, cats: 7, vizslas: 8
Sue 440: cats: 2, pomeranians: 6, goldfish: 4
Sue 441: perfumes: 7, cats: 3, vizslas: 6
Sue 442: akitas: 4, samoyeds: 5, cars: 2
Sue 443: akitas: 3, perfumes: 3, cats: 9
Sue 444: perfumes: 10, akitas: 6, trees: 0
Sue 445: cars: 5, children: 9, perfumes: 8
Sue 446: vizslas: 10, cars: 3, perfumes: 5
Sue 447: children: 9, perfumes: 1, cars: 10
Sue 448: akitas: 0, goldfish: 8, trees: 3
Sue 449: cars: 7, akitas: 8, children: 3
Sue 450: cars: 4, akitas: 9, cats: 0
Sue 451: perfumes: 4, samoyeds: 5, goldfish: 6
Sue 452: perfumes: 10, akitas: 1, cars: 7
Sue 453: trees: 1, goldfish: 3, vizslas: 6
Sue 454: goldfish: 8, pomeranians: 6, trees: 10
Sue 455: akitas: 5, vizslas: 8, goldfish: 10
Sue 456: cats: 5, trees: 4, samoyeds: 0
Sue 457: perfumes: 8, cars: 0, cats: 3
Sue 458: akitas: 1, trees: 10, vizslas: 2
Sue 459: vizslas: 6, akitas: 3, children: 10
Sue 460: perfumes: 7, trees: 9, goldfish: 8
Sue 461: children: 6, vizslas: 4, perfumes: 5
Sue 462: vizslas: 6, akitas: 8, perfumes: 9
Sue 463: goldfish: 8, cars: 4, trees: 10
Sue 464: pomeranians: 8, cars: 5, vizslas: 0
Sue 465: cats: 10, goldfish: 7, akitas: 1
Sue 466: cats: 2, children: 1, cars: 6
Sue 467: perfumes: 3, samoyeds: 6, cars: 0
Sue 468: samoyeds: 10, pomeranians: 6, trees: 2
Sue 469: children: 2, perfumes: 2, pomeranians: 4
Sue 470: cats: 1, perfumes: 5, vizslas: 9
Sue 471: vizslas: 5, perfumes: 2, akitas: 7
Sue 472: samoyeds: 8, goldfish: 6, cats: 1
Sue 473: goldfish: 10, perfumes: 9, cars: 4
Sue 474: samoyeds: 0, cars: 4, vizslas: 4
Sue 475: trees: 2, cars: 7, akitas: 8
Sue 476: vizslas: 3, perfumes: 5, goldfish: 1
Sue 477: cats: 7, cars: 4, trees: 1
Sue 478: vizslas: 8, akitas: 3, goldfish: 0
Sue 479: cars: 6, cats: 3, perfumes: 2
Sue 480: goldfish: 1, children: 9, vizslas: 3
Sue 481: pomeranians: 5, vizslas: 1, cars: 10
Sue 482: children: 5, perfumes: 5, cats: 1
Sue 483: perfumes: 2, goldfish: 7, trees: 6
Sue 484: akitas: 2, goldfish: 4, perfumes: 10
Sue 485: samoyeds: 3, goldfish: 0, akitas: 1
Sue 486: trees: 8, vizslas: 9, goldfish: 0
Sue 487: goldfish: 8, samoyeds: 0, trees: 0
Sue 488: perfumes: 7, cars: 5, trees: 0
Sue 489: vizslas: 3, pomeranians: 2, perfumes: 5
Sue 490: cars: 5, perfumes: 5, akitas: 5
Sue 491: children: 8, trees: 1, pomeranians: 4
Sue 492: pomeranians: 0, akitas: 1, vizslas: 8
Sue 493: akitas: 10, perfumes: 10, samoyeds: 8
Sue 494: perfumes: 6, vizslas: 4, cats: 6
Sue 495: children: 6, pomeranians: 5, samoyeds: 4
Sue 496: vizslas: 1, trees: 5, akitas: 1
Sue 497: vizslas: 10, perfumes: 10, pomeranians: 3
Sue 498: samoyeds: 3, trees: 2, cars: 5
Sue 499: cats: 6, children: 3, perfumes: 0
Sue 500: pomeranians: 10, cats: 3, vizslas: 5
"""

    let lastData = ""
