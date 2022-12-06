namespace AdventOfCode

module Day06 =

    open AdventOfCode.TuningTrouble
    open Xunit
    open FsUnit.Xunit

    //                            1         2        3
    //                  012345678901234567890123456780
    let exampleInput = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

    let puzzleInput =
        """
wzsspbssbhshchmmrmprmrfrsfrrjhjphjjtppbfflqfqppdhdbhdhbbjgghgzgsgfsfhfhvvwcwhwwnppsbsggqnnvvtffpssjfjnnltntdtdddptprppjmmqssrlrplllfrrzggbmmlmnmtnnzddfdgfdfsstbstbbzcbbcjcvvfwwwzlzssglsgllnbnmbnmbbgmmppmwwsrrzqqvgqgnqnmmswwrnnbsnnbdbwwqnwqnwwzzqwzqzjqqwbqbccjcwwjlwjwppqfqjffwddrzrjzrjzzzhchjjlqqrggzvvnlvnvjvhvnvjvbbvdvldldbbwddtsscbbhccmbmbppbmmgjggcjjlrrwjwtjwwmffgddwvdvbdbvvhchmhmdmwwbjbjcbbcmbmmslmlslsjjrnrjrjpjcjgcjcjbbwsbsbfssjzsjsggcmcsswgssbbqpbqqdsdqqztzmzpmpbmbvbjjtrjtthwhbhnbhnbnssvpvsppgrrcwrrmqqhpphrpphchvhphqhbqbtttllsrlsrshrrmhmvvrmrnngbgnbnpbprrrftfjtjwwmrmvmcmvmrmwmssrccfbbhppjbpbwbqwbqbzzfmzmtttjbbvcvppmwwrbbzsbzbfzfdzzhrhzhvvsqvsqsnslljwllqlwllvccnttzhtzzpspqspqsqgsssvnsvshvsvqvnvlldbbfdfmmpbpgphhpqhpppchhbfbrfrllntnznrrhdrdhhdhjjgwwqhhfssjbjwjqqbffvvcrchccwmcwcllljtjrrqgrrdcrddvfvwvzzjqqcffgjgzzdhdwdssffjjhccggmtmltlhtthzhvhlvlddsfsbbtjbjcbjcjzcjjpfjfzzpnnbtbqtbqtqptpnplllbzbllhbllnqnttpddqvdqvqmmjnjggnnnhqqfcqcffqppdgpddvqddmhmrhrtrmrzmzhzpzlzqlqpqhphrpppjqqppzbzmmjgggqtggtjthhlclnclcnccmcppdcpddqrrfgrgbbhwhdhrrqtqpqjpqjpqjqqmcqmccnngqqqmtqtssnvnzvvpcpwcwwqcqqqfcqffsvsmvmttttmrrzssghgssjmsmbmvvwggggzhhfvdpgjmmvzbfjghqhrfbpmbvjzwvfmcthrqwdhghpwsspmhpqnmwhjzpnlzfnvhdnnrqwnvctbmjqzhqrpjlwrssdlwqzmsfrfzmgjhnwwnwczswnhsdbvqbmdlvntsdrhrjjcjjhpbblgwhjwdcdjtpvtmslwvncwdjbwzvbpzbvddvssnrhtshrcvnhqnpmjzfswqbbrztnwjcpflfbhnphfwmjvnvtswgfttgjcqcngmmwjlfsprwfcfwcmgrgbnqmzbtzbtbztngvrzpsnrzvhbsdjnzpwwzllgnfdrlwpmnrznqsqcmvnfbnhqjddvcjmtgbpbmsgqdqzflmlmqncmhwltrmdmgnwpfwddrdpfhsgsnggchzjhgpwrsmdzgjtrgmnprhbwbcbpzbdvvstfqcnqzbdjqpmrdbtgcthtclftghhmnrzrjqqsbndhpvmdpfpwdlhvmczvdfgvpqclssvlhqnhlcfnfbvtspdzmgzdctvpdcwchtqhpsgmmblspjdlvgblbpgrfrgnqqsphcsrgfsdmpqscbjmnqrfbcwfthdtswbzthpnvsfbntnbmmgpfzlqwhppvvdrmwbqzbgppbgsqmjfqtmntgwpnccthftwdmvwmnchlbjhsnmbhndczbrhhjpbvnjdzrcndbbmfwfwsjwfgbqhwhrsvlngsbhhlrdjzzbmjpsqhlpzwcsntjhlmngblspmsjrjwsjsrqwnrcwsmcsbmpjwrthbqhrschrmrppnnbmjbvjzlmzsrfdwqlfnfjljftjvzsqdwlhbblqcdlqjbprpcllhlhmwrbrlgfrcqshrtjpnmhljttdvpfnhdjqvjhhfczwvbzqgnzgljcfrbpgwnfhfchwzqmqqzpbcdpqmnbrppzblnnzqrfnmgtljwnfgzwvnjppdbdhbznvpgwhbdjjvlspgwgjsmfsvllpgwlfnnptmwnfsshjjvqzjwrqpvmsphpmftqdllqqdzcjwfvpftgvspprdwvvcnglmbpntghntdwpjvvsppgjvnbjgvtzchtqtwbddncsbrfcvrnvlggvwgmmnfzrswnzjrwthzmdsmzqmzsnrqsnslnhmfqljnnnzshqfqshrhhjmnhdgphctswdbhnrcgnzmmzqpjqbtdfhhltsmvvtntbgsznshhsghblhlhqpmdcfhmnfzvhgnfsfflcfwbfqzmccrjdpfvphtqbrdnzjfmfhbzqcpdnjdcgwprvchlzrcvrghgjqncjvnndbcshntrfsbsnmjlhclnzpfdgztflcpwqpnvlscfndwqzfvcmpgfncszpmwcsrdbrrhdjvmthslfvmlgpqhlgwhqnjljcvhswbsbqfrfhvzwjvdmhzsgbmbmfnbpclqdwhvrlpppszptjvwtvdmfltfqqgjttdggcvllblnnhjqnjzhvpgpzzpzwbpjqbthnjjlmsjgjzqwnjlqrcdmmvsldtcrzqdrcmwqhnhfghdlmzwcspgmlpzhbdsmlwlqnhhvcvdfzmvfwpbfmjtdllprfqzzjpbrshdzgspsrlrwrhdpmznzzqngwrzqpmtwgbsswrnnnfctjhbcftnslsqwjvmfwfdvfqcnsvfsvgstgbzpmljjtlvtnfsdzpvcgbjqwgbgzqbjfgltqvnhffflsbjzfqfrfbssrvvwqvqptmhrbgllqjwptrzgvqgccsrvbgtvmzfzlmtqrgfwhzddsptclbhjlwqfntvjqdvcddnffmbtqrnsvtmlvljcqdsrpggcmqvmmlzwwbgznhblwzjdppvtqzjvcmtfzhdzjplrdbrfrgzpldvnsgqlbwbmfvrbgwzmjmmqdfgwtbgtzmdqnvqbwvcfjhddqvnjtjlhhjnltbtqqvblwlmglrqcrcfjvdntrnqzzbmrjqglmrdcjgnshcghtprjqqsdrmgdnzmzcfqqdtjtrqgtqtgrpmmgzjtcrznmqccjbdpbvrnnbmbzvgdcnrczbctbrsrrqrjnfcdpzlnngwvcdtbbgssvhpptntqdzhcqtlpvzjbfgzggrgrcgtdfjbwdcrpztnfcdbscnlmqmwcbmtnddgbmhwsgvcfdcmhlsvtnqtmrnsjzhppgwvzlmhwwmpfjzrfbhsgntzrdhwswrnfmmmczqrvdrqnhgrvqbdddhglwsftsljvgbnjqfwfzsspdqvgsnlgfsfpvdrjhzcldtrmjjrmdhvvfrjldhhtqnvsvlldjpjbpwstfsmrpmbqbnnpvqtbgjvblthbmwqtfcfgnjscvtbvlqcmlhffpzgjzfscsqwnhrjhvbrrzwqvbjwtwhtqsdbssfgncppnsfgfltdcbjqjzqqtprsbvjzhmchnltvmbsvpvhgzhfhbrnttsqbcmwpdnwqqgdrjrdwdhtzwsmcdffqgsddvbzfjhtfhtnfdbfrwmdtcqshfjrcpswzcptgwgmctpmzjdbqlmqwthmnfplmctpsslcsdtqpqhjtmjdnmnqnjgchwstsmtpvsmgpsbfgwqnzhrdgdvcdlcldfcmjvsdldgbmhltjhczffwmzqssnhfnwftfgpshntjbpjdffjpcmcpwhclrrwqcqzmntjglzgcfrplfpvprtpvpjdlcrfwrtrzdzmhsrsmdcpqqrqgvfpdbmzbzqdfhpplmgfrdghclbclgswvwhhdvcpmpzflpffmptcrwglftztccrpbrvmpnqmqdgjgrrlbtqtvgcjpljttwtdslqjqlsdpblgrqbrtbmtblfbqtbvsqhpqzpqfhjqpmjrmcvqmsbbpjpdncgnjftclbltwszrrfzqbjcdtphcvpmbhppvwjwlprgmghrjzzgnvzlvghnjbzqjpdgzfsnjchpbzqdzpsjmsrvvqwjcpwznlpbjldlrdfqtrzhqzcnpjqbbbf
"""

    [<Fact>]
    let ``2022 - Day 06 - part 1 - example`` () =
        exampleInput
        |> packetStartsAt
        |> should equal 7
        
    [<Theory>]
    [<InlineData("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7)>]
    [<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", 5)>]
    [<InlineData("nppdvjthqldpwncqszvftbrmjlhg", 6)>]
    [<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10)>]
    [<InlineData("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)>]
    let ``2022 - Day 06 - part 1 - firstMarkerAfter`` (input, expected) =
        input
        |> packetStartsAt
        |> should equal expected

    [<Fact>]
    let ``2022 - Day 06 - part 1`` () =
        puzzleInput
        |> packetStartsAt
        |> should equal 1142

    [<Theory>]
    [<InlineData("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19)>]
    [<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", 23)>]
    [<InlineData("nppdvjthqldpwncqszvftbrmjlhg", 23)>]
    [<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29)>]
    [<InlineData("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)>]
    let ``2022 - Day 06 - part 2 - examples`` (input, expected) =
        input
        |> messageStartsAt
        |> should equal expected

    [<Fact>]
    let ``2022 - Day 06 - part 2`` () =
        puzzleInput
        |> messageStartsAt
        |> should equal 2803
