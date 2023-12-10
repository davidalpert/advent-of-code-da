namespace AdventOfCode

module Day01 =

    open Input
    open AdventOfCode.utils
    open Xunit
    open FsUnit.Xunit
    
    let exampleInput =
        """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""
    
    let exampleInput2 =
        """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""

    let puzzleInput =
        """
shrzvdcghblt21
sixdddkcqjdnzzrgfourxjtwosevenhg9
threevt1onegxgvc9flk
7dmqzksnlcpbsqkzqlfour1four
4seven9gdlnhqxfseven94five
nldeightwoshgnsjnzmbkbxcxltsqtstrgdmvqvxbfour6six
87mmlvfr4
six1vvrlxx8two
znmfvdlhvjtwo9three4tzjqcfcgnsevenccvnsjczlpm
5sixninesixnh
three1tbtwo
five9seventwobqsjqzxkptkhsix1
74fivemn
sevenfivetsglnine8three
spnvnfkbv4eightlmdkxmsfour574eight
one4four2fivedcpqjmgc8
two6sevensix33
krc34five
doneight64qgc251four7
vpjthc6
threethxbtffhbfourfive3
onetwo5
1sevenngjqsfrptq8stgzxgjfp
1576sevenninefhfqd4
nine4nine9ninefive
zp8sfveight9twotgbvscb
eight27nrzmfsix7nine7two
gsfivefivepmq9nineftndvlcph
znggdvvkjthreethree79eight4
7twofivethree7ninevtspmbfxnr
four9twoxthdxdc7two1
8ddcmt87xhkqjbgtmkpjlkhd7sevenfour
4four6
seven5tmzksnsv9six
6fivesixlxfrqbkfive
seven9one8jxqkhbfppt
hxsbgn6four7
7pglchqndpnknhvcnkthreefour2
bkkfjq6dklbppfvgsshgrbmtwo97
66eightsix3nine652
six6zjjfffhrflrvkjgdrstcqpqt1eight
four252bqdxzzfttkq85tqnl
pbgjmdfxjvzdxjjdtwo5sixtwofivesxdcxtn
jonesix2five4snxcbgmx
62rceightsix
fivenine5
nzcxfqhvninefive3nz
4474rcl
1bjlvthree1tjthreethree
bgx2six8vlbdmgsrkone9bdgzsfljlk
seven52
fivesixtwo1two
njznm98
3xhrsfshnine7five
four9eightzcmsevennzfrxtsixone
jjsxs4mgqnstgpreight1
bhv9kr8three
sklnine1six4
five52734eighttghkkszrdllrr
93one
qrlmcmfgsx55onetwo
86dnnrmms6gxskhcghct97glkrlzrdrscfcld
xxb283cftvdg
hxtwoneqpmbfgkhnr6three86eight7five
zjfczdbpgxtkffkl5
fivexnqvfiveoneszqkzdfclxkrj8
8onerj7mhl
6fivefive8eightsevenqc8
eight89nineonelmfst193
ninetwothree5six5gd8
eight6dvvd
sevenztbdzndhv5threefour
two8onebppzkln6
twothree54six
seven8ninetwonndtgkj1
cpjckcfnqrrgdzfbrtbdfive8vkl
6six5jzxqqtrpflpdqbkgktsvpjgrhggsmvltwomq
sixhbft5lnqfseventhreefour
23pqkgleightxvscbnxnp55rb
6fivesfhqmzm7qkdvjss8
twofivefiveeightdcl4
9fourfourdqfqhpbbkgtchk2nrvmrghnkdc
sixlzbpttdhlssixdhmvrrgxj6
glphjgxqvbjcqjpdsixlsgd3
qcghmvgbmj9fourfivepztxbjpnzpbxzckgnhd4
kjvoneighteightfivepsbkgdgpndxx7six8
five95oneeighttpdk64xxndq
1vqxhglhnhrpbnlvq
sixtwo34sevensevenrnflkfxmlthree
pbscshhhpeighteight668fourphkdcrjrf
threex1jpdsmc
4pnhtjzlfdcgffour3
338
62six7nn
1fgq3onethree
7eightzvxmvxbq965nine1hr
gsdqtgkhlhdff7fourtwo4threesixthree
918hjpgbz6kbdxeight9one
7dxhsk9ninejjbninefour
one2mblcjnine
9twoc
fxrkzvpkb3tdvlxxchbfsixbmmmctknxh1
pbzdkmkxczkgh5xsixtwo6kfgqsgx
6eighttwodsfppgxrzx9threeseven
zzkbhltv7ngthreetwofivegdzrmllnxp
sgfour67three2seven3
sqglnvlrn4sixmdnineeightthree
1kbkmkdnc35eightxbbknllscr
2bzrg
3onenine
threevkzrmdbnbsxrmzjtdhbeight12rncbzmq
sxcrgkvbrtwo17six527five
9rbsbcjp239jvlfpn5jrr
nine8vjcbjcxxprdcf4
nineeightsevenchjczgdvjvtqlkjsqvrzxxvlqlp65
hdgqninefive9sevensix3gzb
6fjv5
pmgdvcdp1fivetxkmjzone
eightsevenklxgcj8sixccqlnxzmbeight
bkcdmscssixsixtwozbdzcpgpbsvf38
ninefourrcmrfvszsqhpxcs89onecnzjvhrphs
54plhhmnmkvonethree2ktzncds
pkglbgdplgvlstpgpnsmlsixcpmfxfbfbtpsdxninenine4
one6qhcqnkqc9hfrsqpxmcm7eight3
threepdtshdskxgeightjtdpfbtlnph91
fmpjgjqpgbpx8two8jzqchdglhtfiveeightpzktj
njqhghlvm2
dj3
3j
eight9cgbcppzjbl1tgjhm
fvkhcvflpknqnh9ninetwoneqn
threemfour3eighteight
qjmtvcbvzzgxmmpdvrfv8
tlzqpfsix1fbhgdvhblvgk
eight451qvnhv4
7mxcvmpkdvdsjqdd
6ninetwodmbjfour52
jdq2sheight
seven2foursixvhlmsjld
ndoneight361
poneightfourfourdhlnmlnpvnsixone77
6gnxprrthree9
ktbhqxmvcbczfspfddnjjcz7
4nineqsk5two5ksljzqmnrrhftqvmvhfqvxtg
cgllkmq1fivevlcxkmj4twohpclqj
fivebdkfgmlzckstbmone58fourc1
6sixninebkmlgpspthreesevenhfrhtfqnl85
27fivezqskxvqqp7
7vklrcvmq2nldrglnlkmdjgzone
4fivemzjhhvmm
76fournineeightthreesix
threen45nctdxmgd
eightfivetwotjbsvvrfourseven4djjfftk9
lrxjcgdxhtprfpvn9eightsix
foursix7lbjqxb1lzsztxcrhx
dlxftspksone9mfive
ninefiveeightone2
nzbvf3ninexsixsix
1eightjsqbvpcfsevenvdkhbxzfournine5pttjffj
6kxdmlhtslf2rzmbrdrvvqmtceightwojj
3fjxhq
j9
lj7onesevennine7threeeightvmgvtzcg
pmmrbq4
seven3fourq81l
2four4gqdpczpg
6sevengzdxmfpflfivefivevgnhspxchsfive
3c7gpgbnlfhlrthpmtsvpctjhzf5seven
kgtczfjdeight8hpcmkz
one5xkhnfvjh92ninevxkzvkg6
nzp2
eight226three5sevenhhxhqxns
9hgfj85zxvppflblvjlq
sixfour8hzlrbqjtjcgrxseight
6tlxtwo4258kbdtx
ninemhvrlcqvdthree5
8xfmjzp
nineldrbpxqmhffrjrcx11
mqvjnkjr6dx8
575cfbnine14
threeqnxnfjmttwo7
pvmxfgvhrlqn4fivefivesevensix1znt
sevenphcrjjhnfive63three
sixxdjplseven8seven8qlxfvgktf
hdgnr6sevenddzptcvprbhvnine
onebrhnsqbnzvmckmtmprjkbfvn6
hxqdjrmr94five9qnkl5
g3fourfive6
22eight6vsmtrxsix78
jrlrfsnine9
tqpkqdtknvfj2xgzrjpsvgphlbtthree
qvczktnrfvmgpznspqtsevensevenslmjdqfdld5rrcfour
rmgeight36sxvxzgnine7bs
sevenlt2fnqjtkvrj
seven9lhfzhmhdonelxvxfqdlxlscjbqgvnlbm7
3183six4seven4two
bpl12mcdpzrzbph9fourninercpqzrn
eightzltzdmcd4615nmone
5bgzh8hrrtjhnine62
ndnzpnptz6twokdvg563fiveqkszkccqc
6npdfqtwo
onetwomcxkncm3mjsrknhktgdfbqjmone
sixtpmdfmrjone6six
7fourthreekqvbhfourfivenine
98337
pvkrzdzbdj9
8krztxtqkqksixr264
5sppmxxkdonezfvbdcsix
dhvmctjcjfivec7pjmtnlqs
dgzl9sixggxstwoneprr
gnbmxn1cjnq3three6rpsrqmtwo
176seven17six
sbvfive3six
8seventhree64pcffive
fourgvhbrsfjzsevenfivenineninesixznq2
nine426six8zk
4sevenfpnmsqlhllrxrhjonesmgfhzmhvstwo3
fivesixtwosevenlfbpczfv3six8
8fourltcpdnhg178fourglz
rdfqcdrxdc6twotwo8fourthreeftrlzseven
97threefive7
sevenoneoneszdxd77two
g91q392four
2twodb
jbtfkfourggc5zkc3nineninekv
379eight
hhxcbflcv6fiveszpch46
threexfsfv3dpfourfiveone
fivevd145one629
31vdzxxkxsmznrqjzpqj5onescbck
9onejbnvrbmtgm
thrjvpckm4vglthree
zvhmxzsixfjddvjq1four8lktthree
74seven84rslpcjtpfourcvvqlstmqn
dfxdqr3eighthfhcldhgmjcpbxbzp
sevenbgs1seven9bjntphktdktzhrzpfcfs
7kqkdzncd4sevensevenfourv8two
mjeightwolsk2sgnhgxstfourhbhthreeonenine3
4nts
nslvxzffoureightfour5eightczsptpg8
bcckncblzdsixxrdprtsmbjdzxzlnzfbgmkfd2
8fourninesix4fourseven
twospxvtbcjfour3seven
four2cqkxkbplbpb
54fourtwo2xd
tljfive7bsxmonesixl
8mxd8czpdrgxbtwoeightrqpghngcvt
3sxrzljnzlr9threetknfcc5seven4
eighteightfourtwo8fournine7
jjzxf153mdzhh
3three95one8vsmzkrlvhq
ninefour4pzqzntwo2nine
onefournine8gvp
45vmbrpblzjxthhonesix8
lmmrxm5
13mhm6zxmkhbcsixhz8
mhsjcprhdh7ninemgrxnqzt7pdtxxvsix
twothreesixqcjtzcxmd3ninezqrvnzlxt
hreightwo51tb27fivesevenseven
fivebktq86nine5kd
one6nglzxzkzrmfl12nine6
2th9mms4six3
msix5bqbtwosix
four1hfrmxsix3five
nrtjrkkfour6fivefour7fivertjnxbbzg
jmjqmxrtjr23phttwo
79
6twones
sn6fivesixfourhhjhrfthree
xdfxgzccsvonefivenmqleightfivel9nine
78sevensix21
rkmncslfr6hdhtlchnznineoneeight4
seven1496
8fivekhfrfjtwo3eight9six4
jnrtwoneslxrgkeight67fivetwolbhvfftqjhl
eight6eight4two
dhcxc1
fivenpblbgfive6moneighttzj
onef5nppkqcl6mnzhvsixjvcqpbtb
7tjggz68four4six28
zfntm7
3sgbsbn2fivefour3
8bmtcgtqcsmdktzmj3
617
7sjsshfgnpnjskhccdclrxmnl
trzeightwo1krbkxd9nslndznine
ninelmbxldnb1qfqkhqkp
3pf8vlmgtfouronefx
five7seven6
zckdhnzrppb3kqsix
6fivebgf6fourktdsqdxfqd46
four7nine5two4dkkdhvcfsf
5vtqrhbn1lhxm7one
three13fbtqxvmninegp
fiveseven1czvlmlncthreerstssbjdoneightpm
2lpdnfz3vghpszmtxnineeighthkfour
455hdvfivervnine
three64
six6threefivethreedbvvftm
667sevenzjxmrmpxd8
dsix62lrvqkfthree
sixthreefour5cpgsjznfdbtk
two4five4one
1hxbnvq
sevenkrzfvkkrprxzcfgchscbstwo73
nzhtgzzq448pftthpsneight
3197
one59fjdqjd9
pvcphzxnvnhhngl5dsix6fourone
5eightfourfivetbhh54eighthkbvcfrpc
threehvdqjdl45fourrqhqxs
8twonine
8mmc6tmc99rkttglnnxk4
pxnxckqbdhhvhbheightfivefivesix6two
3four2pzx23vhmzhrfourthree
five1four97
9eightpzznbpqgj
rrrgfxlb7vltwobjtdrghc
sgnd8ptgdfourbbcqsix
4chfmgsxnsd96
zthree3
zvvstf6twofivelftpmzkttwo3
1nineoneightxqt
2kzhkfoneeighteightqn
287cfour
lhmgblplseventhreenine9vcpnpvzhjronenhczffqt
5xrjqdjtdqnvlnrkkr7
38dbdthfive
onethktvdnfqgfourlxpksevenseven22nine
mjmt18blbfiverjsxjsktkgz
8hlrjjbvsix92grtmthree4
7threetxjztxseven
five5gfgfdjbkrpseven4
8jdzlvrgtcf8eightzpgstwo
bfpqdpfoursevennvgqt8
8one99
kfvmblbtthjrrmktmjmeight4d3lnctfzsvgmjtmd
2eightgppxscjvdgrzjgc3
5twonineknzone
onefourhb4
eightrxchxccxb7qqqqxzt
threeone14pvfive
nineseven1sixknvkmfkk86
onesmvscnf9ninezgfrps
ffhseven2
1sixrjseven8
rvzthzdtwosix6kzzqssrgc9tl
threeeight5svcrlrqtkmfpgqzhhs6
eight3bt7dp
zqhncxh25eight1
five3vmmmntnjtjrkbvpvphj6nine
eight6fsdqseventxgjkbvrleightfive2pqx
nkfbsdgrrfvsmfxx78onetwo
bsix5eightwox
fph3jzfmt9fiventnpvnxhjxplchjnvlxqkdbzv
kfxbjzcfpfk8chkrtvlrnine6
m7kfjz52eight4tworkpcqt
vmlhvdpxds8two
fourfiveoneeight478
3eightonefive
ninel5
lfqllfjcqztwofjnfnfx5
5tvqksfour
twogdg5fourmhninecrdcrheight4
two5six2
419vrvjln7
pbpmljqjzxghp3tkdeight7
sevensixfiveqmvtfggbqjone72fn
7four93cdgcpxpl86
9oneeight6three
threesixsixhzhpfive8ninetwothree
xlzddrthree2rmtwo6fivedhxsrrdcvpvgscnt
qfncszfsdgzqctdtbpkseven1fourpcmz5
7seven9fivefive
82nine7ztqmftkssm34j
twofoursv6
five2five51qrsrqrtmdk7
four7fhhjxcjtbr
hnbdbkmjc72
sxclpbzmbd4fivezjjnncmqpv6ckxzb2
98six4pkhfzkzj7
611thqkzttfffivenine7seven7
8sevensmqzxxlgpxpjqstsgkmxdgplrplsjxjgsmjdzzgf
rggphsjncpxkfzfplcgrnfgmnd9
three9sgfhdn3rbvqds3threeseven
nine9sixtthree94rs
sixthreebvmxtcqng67one
6ntrhzzkl8djptbnsixthreefourhnk
3sixxsj5five
1cghzppdcfnine
jtmkrvlmhthree58eightqbhlgppghc1
fivefourlpk5jtvrk9eight7gfzksqrv
tjrphpkhfgsix7eightwocj
threethree85ghjsnhgrv2jv
six64
3ptfzxjrtfsix
1583xpczlkvtwo
twoglkpldjthree717
six26three
three5qc1gz
9tworgcbxfourrxgxzzbljcb5mxn
838
four6zjdfd22kxpfhhgonefour7
seven132
zmml5
twozzzgrhseven5
four8eight4fivefour
g76nine4oneseven6eightwol
9kzlggh1ninefoursevenninelb2
three27kfmbdtpxncgpskvrq2
onegsnxggjlvn1six
8fivevgtszhfl2zqsqpnmgdbkfour
4fourtwo
mrllxmbfpqvpfzhghv8twotwo
3hjxpgkxpc91t3
four24
six385hsxtfone
8three9twotnpjkcdbrfive8
nine94tpfcmnzsjk
sevensevennbch64six
onefour99gcgvcfvfnzjtjzngc
fivefivedqdt4
onefrnpc4onejgvtwo7six
4568
tfrmonerljgxsbghm87hmgzfvlcmgbsbqc43
4kbxt9pqchtltg6sixjkzncffrpsixtzdtbdg
29pbpph
2szjrkkqvsix
six2twotwo1cfqzhjrlkbone4two
mdxmcknnmmnfzsbvh4
rzjnjrqjkmzmr4seight1two6
onehpqvvlsnkhcqhczbllbcfl3six
nine655nine81
two76threefourtwo54
8nine91
4threebspdskrp3
pktwonesx4sevensixfournine7
td2one4seven
eightseveneightfivetwothmngvb9one
9pkdcbgfhrf8mjtjksvjlf2
374four9two2seven6
98eight2
brmzpfgjone3
six4mdbthreezdgrktcdc5
mfmvcqdjlvzmeight8
smklgfour2
j3nineeight
fh45peight9nine6
two972fvkfzsfivefbgkktg
three9kdzkeightqmxcslnvhthree
ninekgjkdtcsrqgrmdtshsevenseven3four
2nineninetwomlt
jckvcvtwo7
348
cjzzxsix1hfqxxgdvphppxzvmqsv1xxxsqckhpbsix
jcnlxlccrtpmqqmfour8dzrn8lrzq
7sevenzrcgtldkjbhjt
three55gtrnineztxsdcdcdb2
sjnvr5
gvpcd4one2four2foursix8
mxskkrvdjkvpnine6834hsxsn2
one5fivesix
bfhbgfdkdftjtnhvgbr2zvmcmrpjs
threeeightmbjc5six
8zcxflfkbtdtwotwo44six4cnrvvft
sevenzggv2mkjsl
sevenvx8one55zhkjcxbpdz
zbxbftwo79leightsix2lgcmlpbpjz
one6twoqrfoursixtwofive
111threethreetwonehf
72fninesseven
gsrshxjfhr54jmsgnnninecvqdjtsevenfour
m49z51
2one27two
four9rlhsbfour
eight8sevenoneightjtj
pgv57nineeightxnblzsixxgjttbnzrqscb
3sevennine35gfbqvq8seven
fivesix252one1nsix
3qzvttqnmjb4pshrrdzpm
nvmlzzxfksevenlvngntztwotwo9mhjrjrxnckchs
seventwomzk699
33svrghbnine8seven
six48fourtwoeightphl
7fivekcrsvzdvsix
8sixjtdrl446btjlmfzqlg
g6sixjlsspqkthree
z2
sevenonec6xhddnxjzrn
one1bczmjxrnineonevjkbnine
eightttltnkcllm8snhbqptsztzkdffkcdbsixcncvr7
mnprx46jdttjtxrkhrninefvvphzthreethree
3bjrvbmlpv6threefivehrkcv
8hjkrlmg
lfrzztvtwomkgrtzvseven7
fcx2two
27hhbgl3x
mtveightwo3tcseventwox
nine8four1five4
six56qqvvtwo
fivelhhcjhpfnsvtzmnxdlp3fivefour
fourtsbq3six2
71vcmtqgfpdkx2
fourfiveninetwo5
qsfhrhkm4nvxdxzcnkcknvxjg2two
915dftd69foureight
qrhjdprpcbq3eight552kb5
rsone7nckeight
3ninecmsvkmlk8b
fourfrdgbrdzsfvjg2fthpptfjgzjsnvsxcxgxgcgdcl
sevenkpshht4threemthmfbdp7
sltzgqcqlvdbccvcbkzmmpsl957
75t3
4sevenkfourpxfkgkdpjccxpxrgsbmsrnxngd
2oneeight4mpndtnnone4onesix
six35sixfourfive2
81twokrqg
3dqcrlnzsnmhmzzxnsjsskchxqzvtfrdnkqg
17zzrcpnmshreight
gcvvczmldgh8mnsnpmjvdrpznine
oneeightjhmf9
1hlfour3three9
skjc82gthree6
eight9bszxphftdsixsixonesix
five9xbfgzfh
6nineone
tgnqfiveeight8qnlj
28fxrpdmdjgdnine6rpjdxgd58
gsxfivefourhgznkdhrtkn7nkt59
6m2eightbzndjnfthree2fjkpsm
9nine7one7rgsh
xnrqcllskcrlxglbshvrseven3nvjlcngxnine
sevenpltgmq9csxz1four
7threevbhfgnshtn5nine
gdj9
five6two931txb8
fourtwoone2hcbzlggvfive
7xlpjkrvbtq8qlgrqonetwo4bpxtcgjxv
nnxhcrgplrtrskbjzdd8lfive2six12
bqjdtwo98z
6ninerfqlbxpx
8seven642six
eight54
5two9eighteight7hzkfdzg
twofourninekhcztkhfour23xllcdb
zmpdb7jffqt7nhnmdrqqvtwo2krltbfjjdd
sixfourlxxshzfccgtzkdhjeightfourninehmrhqzfqld7
mblx1ninekfjhfnjtt4nine
seven7mhmkpqk
4rvzsftqscltzzj551
8gkvgs5oneninethree3three
5ldfllvzkhkfive8
jjblh64five1gzvvgcbrhbvrqfhjqvkrqvff
fourpqnpgjnclq7nine
eight4761
nddmqzx2four
8nkgjkkhqztwoeight
hlqtbfrgsfbgnine5qdfzrsnmb77
nineggdbpmhbmdnrfvpv5threefour3
jm4
sixfkxfzvrkxv9seventwo8one
threesix2oneoneightc
7three3615qqdbzsxrdv2
74fourdvgcgdgvxqgm4eightqqtmvone
5h58dmvkxbhtninefoureight
8fiveoneseven7
eight145zxgnzczcdkzrffnbfmsztwo
268nine3
6eight3nine86six
4qhtjbcd9
mblblmkhseven3tgdxslcnine
fivefivelgghthree5vkkfhjkc
4four5oneseventwosixfivetwo
6ninenine8
7ldg64five9twoninenine
3zmzlnninesrfourfivetzxlkcsr8
sgdzmk3lgxjrktsmzkczft
64nqhjdjs
9sznchv2nineclrfdpxnf378xhkjbbplqdeightwos
3sskrbzrpfqbbqtjzgcqjhnine3six5
vq95nine7lfiveonethree
3eight3eightfive4
twoninethreel7sixfour9
bszxlfz2mtmv
1cvhpxjlgjd
sixsevenksj77one
qglgjsh3zrsqqh9onehrkjxfsxt3nine
sevenlllqqkk4bxcgjqqllmcml4b3
bppqgfdfpm7753
68frjqbqmmq2fourncksn9
bdz42pjmmnmhkvd
6threetone2
8bmmdxf1dbnggxgpvhninezxmlzhvzb
48168
gvgkcsvmoneqvlxgzjsixfourshxzls9three
1zrgnghhm
klkrldxbjone6eight6sbglhrhlbnbq9dbkl
9threefivetplxdklc2zrnptzrzp2
sndxcznj9ninexttpfzbjcmbslghm83rp
73one8one
3eight8eighttlmpmh9five7
7vqcddvrgbm5foursix
fivedhzbdsnzd1sv
82fgntsxnsix3xdzk
threesix3qblcvkp
lfptgxqs2six35
eight33jtzdbstcmstjxtbseven8
seven4five958three9
3sevenhpvklhxxvx
rzsb5eighttwoglqtmf4
seven8fgkp
3rjfvpfiveonesfgdbb5
vpkkcrpdthree9threeninesix
3onethreenine7
9sixtwoninetqllkjtmjnine4
2lqsevenx1vsgbmgone
eight2kqsixfzcxvxfourp
gchhhrbxqlfphdjjdfsix4rcpmj
4pfrxtgr21
7fvndztkj
rtwonenine77lsglrqcxhgxrllftvqfvfive44one
two374343nine
eightseven7fthree
6lrrphgpnrlvgfqqzxfdxttdbfdfgjpmtztc9
61threegscxrcstkclln9
xk2dhkgbtggeightthree
5vbbrqfnfp49eightqvn71qpskg
sevenhgd9qxfn8twoeightsevengcqtt
1four17five
5seven5vnsbqpkjzeight7five
62qgxndqlninetmftkhp9
two8threeeightdpzzhhvmr8
kxgmtlzb7gcltm9cxp
8454nine
mtqqxgkvfdthree9rpbvl5sfourthreesix
7onenine
4fourthree
8fvr
1fourfhljbffive
lqltrsvkjsthjkxph3vrfive3vxvlf
xxfoursevenzbthcsdnrvjlnqqhlpbbpj8
6gvch2fiveeightsixmtfps
two7rljfhhdlcseventhree9four84
5lpseven
cdmnine1
oneeight97mxnine79
3thzbhxbcbf
two41fivefiveseventwo4r
shlz9
xxpgkqf9joneoneseven6
lpqvvzdbx27
gh1five
thzrmh2lpncngjrqrkmthlndl2six3
vrrhfour55
665qzpzlhfvrbsgblcvgzbqqkxbdtzctwonerx
five44
kpvqnxnlfouronerp5417j
4vxppbsfive235gqchjfhjsdgq
sixnine2lk8npzdbgcxvrd
3six6eightseventwo71pff
zbr1srxljjseven5ckjfour
one71z3threenvmtcd
six9zdchbfsdtbs
2qhqnrcmrninedqfivejseven8
zrhctxclslhlj24jhhseveneight1
knkvthzrbtxbbnnpsfbq1ninefivesix
vhr28rlrtpjbnn7
xcsjqrptg7one
gzdjq2fourpqmzjs5sixtc
threedzbjqd3568
threesixgcjx3onefourdskrfhldjhm
sixphnfkdtfxnvphxh3
rxldfbssevenfivemxtdbnvgpq6xxmbkdkxkjmth
five7five3four2qkrfmt
4fivefour
fivenine35tzhpqfkq31
9two2fourtwo8nlgzgbbp8
1threeoneqglrlnmcblpfhgcfhggm
7onerknmhpmonejzfqjng
nine81twoshgxqt
28dqfptxseven37jtqrklrqc
41two3
7gp
sixdzjfsv2sixeighteight
9three6
3fiveseven83tvtxf
ndtwone562kzfhdrhgcjv4two
jzvtjvnineeight42keightclbtddmdffsdttc
eight41mggmrzlkhsevenfoursix4two
rkllqvfjz13sixthreetdztlhlcldoneightj
7three4fbccblfvninelhvlthgh
btwone96two1onexconeonefour
jcvxrvkdsb6five
578947five
1819p
lgvxmnjkgmlkhkzrone6gnbgkxrlpspj2six
ntzsdxn8
mplcrhcz8
czggpkxcfthreefourjpcxnine4pvfqxcvh38
9dsfhtnrjv16
2ceighteight4ptzneight
2two8jvsgrtrht7lhxkdztqdhvcsdm
619six
cqmqkhpxjtwofm9one1one48
2nqbksgxxfoureightfiveeightbbvg
five5nineskphfdgxbp67
gsevenqgcsk9ninehlccsxn
pnqcdjknth94three4sevenninefivegl
eight9mnsb6
fzsfcbbvvqnksj85fbfour2cn
1kjzphbpp3spnbzhzeighthdnrdq
4nsglzjbk1
threetwoninethree2ninehm
35oneljcsixlmdxpxk1
jsix2
tgkfk8ninestnk2eightoneeightwotcs
chhflbq1xznbmj4oneightkc
3eighteightfivenine
six592bzj
xnqnxkpseventhreettv8jkds
honeighttpqxdbhsevengvzfourd4s5
sixeight8
pcflsgrvslxgmtqm6oneeight69zhlpddq
nqgeightthreelzcppk2vzdjrjqqx41
dqdcmtzbqeightfoureightthree7four3
ninesixhxrncgtwofivetwo5eightrvq
kzxgsfive644lthreefive
nine2one14four7
59gjc8fivetvjcjcq
lczn92c
3seven9tprznnsmznineqbpchxlnbqkssgqx1
3m22nine7
3mhzbqtscqrkjzjqtklxdn1
61pnjkfournine5
41one
99twonzsjsqk7fivedjlcgzjrfive
twoninexngksevenfive7xxrxpvq
1mfnggsgvqmplg5mrqmzsclpmeight
2ddgfivehxddfxbdfsseveneight
4nine579three13
fiveonethree44one
11four
cgqrbzdl6mnmvh6
qmzeightwosevenflbh72nine2
six59one9two1cvzslmdch4
fivesixtwo8
sixxvczqbbvctgmsjnffmfourfbr56
6seven5hndfourfcnone5
qdnjmcxtfgpk6eight
three24two7threetwoprztmh
foursevencfzcdzksevengh6nsnine3
sixseven4lmqshqzmk
xhrjx9eight
twodmcxbsn924
2zmcjlhqj4sevensjlnjjx4
pctwolxqvjsjdlfx8eighteightsevenhq
sgzzd5five87threershbhmqlbm9eightwoq
tlzmzt6jg7fiveeightgc
svlthlbrfive2fourdbfp9trvbjthvvthreetwo
nlqnmffxlsblnncqgcgsnkt7fjhfksvfxeight
hljgks3hfour5lzzpzpjpgdhzflrnqpfzjsfc
9eightfive
eightsb215qkphjjdrlg3
four7three
nine2eightrqh21
tnine8txbm121three
two1h495skpvbzsixnine
73five61pzlscqn2nine
threeeight3ffnjgk2mprkn9ninebqlvkl
58four
4pzb35twotldtcq363
jnzdhlzx4fvsjsffsxn45seventhree2
nsfrqntjhpq4qstwotwoonehs
fiveonethree5rnmlz
seven18jjcdcg
threesevenqqbrkbpzeightninethreefive1three
eight98
eightonetzggcjvpml2twoxlznrn
4dmndcrsc
9oneseventb
5pkf8zzmvfive6
rdlnxcx3nineqjpldqtwodkctfnn
8mpmgzgshszbcnrpzcszltd
2jvkc3
rc6clvgbz6bbtlzeight5
frnhbsgzpfeightqkt2ninethreetcnklsctc
threefivevgzhh2
tjoneightone9three9dkbnh
hv7hpx
7onetwozrl7fbxhqgrbtoneightpzv
3mtdnphsvtrrfeight9sevenfivethreedjcdxg
pqjcfjsix4tsxcthzbqzhklqncvb
fgrzvnnk3cfrxp6tsb1
czfdsjffqqrcgffx7seven
56jdtwo
sixxvjjmpcttwo7ninechbzv
8qckktnslsvrmpctknine91
4two9fivefshxvdfbdr
7sixtwo4rjspztpx
8onethreestjxv
3lvhvlnthreeonetwo323
hxjcbtrtcdgvg22ztn6xxnsdbffour
twolzone34crrcpcdb4
fourjhghcseven9gtmxrpbpxthreehkpfour5
qdrxfdlqtf4342threemlpb4d
nine6nfnrgd52jnqfourbzkgbxlvlmr
sevensixonem5two4
64one1nmxbmsq
fiveftmrkpmjthreekvsfqjchvf9two
ninepffbfjhjtg4grnzztqnfiveseven
1gkvsgsrrxvdfdtsrlgfthreedsrprjft3
foursixsevenninepxllctr4khsfxfvnine
onebfpbqmpmm6
cgkblkxtr2thqsixsixthree
qqfkvsntgf687
fournine2twotlvqhlrtbmckqlljhssevenghhttkvf2
ninenrvhhleightkxbzcmx2sevenvzsncp
eight766sixoneseven
8eight61gsxklbp
2ptzkzxsnsix9one
six9vfourseven5brmclpeight
821
one6hcsxp5njsthreesevenfive
pcf2
eightgxsjc456
threeseven57htjtqxbbdh
6twomsbq
gzknkt1twopccfh
7vpzmnt
8fiveqfzgbnxgnh
onepvtddfzn2three
5ninesix85
onejkgppztbmrbfqqrjzp24one
k3six817dvvjgqlnkd
zzvkseven9sixqxr4
lpgdnglg3threemtltwo
three9qfour8rxlzjllnskeight2t
sfxgmjzqs2cbqtwo5
one36
2tdknkgfhpj1nine9rtsjrjdlsix
59jvsoneninethree
fzjvgpsqrsninenlpxzbone6
fourkllsixbfive6fourone4
seven52zchsttccn3three7
6ncvnflrdqbfourx64nine
2vrhdchccppdseven4qkdghrjrxbdlfplc6
5cbdfvxxtwoone38dmjngzqlf
fsqgt6jqdqdj7cfdzrfd
9zqfiveznnmghtrtwozqbeight
twoninexmxnpbvtkn1tjzdsxjvbd
6fournine
15twotwo35kbbpcxbsmb9five
1two29dppbcqkxgq
six7sixpqqmdjlcgzrrnvkqfqmllqlbmfnvlnzjcs8
jhtgltv2qzhnkm2three8nine
mrttwonetjrt2eightoneqqgvllgpqqbpd
hhtnb72
sdzhtjmnkdtpfsfnsnk7rtwo
3sixsrqsjsts643five
338twomnqtwo4cl2
hgggrn2kvgkvrhngxx2xzmqsdpzbsxfqhg
hctwonefivenine4tnsixsfxlvppm17fvq
six3blppkhpjtr
trrxmdnhzsix47
5vsrhlvqpttwojczmeight
rzgrzbkk87nine
trgfsq1ninefour
lxbvlbpjz4eightsix
6zmmvchcgjqsrthkgc92
9thfthreemxlbt
eightpkhgpcnc8eightfive1hdtcjjdcsevennpz
fivepprkhhtg9cdfqhsqfivejsrxzvknndnvq
qfvjvgeight2vbpjnftcttwonegn
twolnln7pnlsxthfjpfivenine
hlseightwoone6qvcrsrttg1three7six
8xcsllfrlx6one7eight
fiveshnmbngmrnineqjjlvdrfnsmpdnine4c
three2rcfcrgmrvsjzcflqkb6487four
threelnbxcrhglnine534
5flztcjjeight7sevenfb
qgbplxgtdkrqqsvjntwothreens48eight
five34rbbxpnkftntrt
nmjtwonek4
cdnxnppnvthreexcsmbmcslh8ninefive1
6lttndjpcchdpgkfmmf
oneeight88
three4xctmtvvcp3sstxzfmgnine
6fivefivevnpnpknljlfp6lone3
one486twojknbmsqthree8
fkxfourfourlpslcbbtk29
6seveneight19four
fvcseven8hzlrgpmgfj136
sixfour9tn1tfmzkdjxxj
ffvmfqmsnineeight5
ninestmzctrthreesixqdbbvtkjfxpjpgzvpthree7zsfmsvsgxn
rz2eight6655
vhzqvbxr6eightninetwoeight66
2fiveeight7seven2
5six6vkvdbjnhbdht5rqhkfour
eightrntwocdchshssevenone2dxhmccpn
three4gsgleightxvgjpvqshlfzntwo43
9four5onebqlcmrrkvnine45
mtkkbxbfour5onemnxcglq
one18482sevenpkgxmfour
two3threexggpjbncmkvmd1
oneseven9
679b
7vc9threenine3
one35x
4eightthree9
1hclxskfgjfhvjtgqsztp
eight38d84fm
fiveone6five1five
qjpptvfsrs614fourssdrxtpqfz7
9qnrkp
six4one1zxhllkcd
six1fourtwormz7vxfbhmg
one3jxcjzz9
vzmtgdhpjneight83fivepcd
onexqhvqpvmncljmssix4twoseven
threeninenine2hg
bvjvgtdhjstsevengtbnqf4seven8two
twooneone5lrjpptxzd
hzlb1
nhrbfltwosix22rdjthctone
seven7eightthreepgj
fourfive45
nztwohnx3nine
fourhfxgmkcgbppdjkqpx2ninezdkbncsevengbdxb
sbzdcpxnine3nine
8grdrone78
1eightjldlllrxl2
588onethreesixlpdkxfc
vvvjrnzbfxgzxqlxx6vcqkkftxkdttnhpzrvtwo
3llpjrhrone5ninemdqjnpllprkphk
44pmxlrhsdsvsfxtg6
nine841jvpl
eightbpsqrkzhqbhjlrxmzsixvvmgtrseventwo7oneightjbx
znhjtgjk1three8
5xcsvvvzrqcxkqj346oneprztsfpdld6
67six1four1kqj
qvqlmqtzcj5
ninejrrhddqfivenine7psghccdhvfive
pznvtxdklpvqbsevenfiveninehzjfvrdxz8
eight46xpfdcqbmprchftkpfive
bkdvhbhlmn1
99sixv
gdoneight26pzghbjfeightttfbvhltwo8
three5nhd
three5nine
four9csdvvfdbkseven26peight
sevennszdbkkfdndjtzzpjmtqbxlkv43vxlnnrn
4fourpxzkmfksix39
9hz8crczlbbhkrsjblzd1q9jsbf
443twofour88one
2tpmlxltgsix
83835six
8twojvsgjtqvxgsevennjdvdbqccmgcldp
fivebpknnpxnjmpxntbhkjsp4fivefour
hvxnseven6two64five2six
5twoonefive
jzkd6hvcmfqsjztsixfivexlfrvn
xfbcskone12jvvbflfn
ninellrbqhshsxeight7two
4nine79jdkfkgvcr1
eight8twoltnpxckbqxnfbxtthree49
ctkfveight8
nine671seventwotwonejkf
5threesix
4sixsixzfmtzlspfcseven
4pqnrmqlscq1fkzxngndmgseven2
27four7znrkvxhvt6
lqkljjh79six7seven
2tlb84sixfour
onesevenseven5htsdxvfctkdgvqtwoeight3
66836qblqgdhnine
75ntphbdbpgktwo
jheightwovtone8fourtcsbhhntkq3nine1nine
5gcnnbjcgqn7
1nnxkfdmxhsqqttsfsgtwo5three
7sixtwoeight
9lsjcmcvqlconezxvrrptxlxleightlvghvxjgfive
one6xxf1bjjnkfeightwozv
4rqxnflktwo
5three1
vxqmbgjnr6one
eightfour538
gvjt1onetsevenonesixfive8
fourgtwopbjbcvgtwo3one
68four4htvj8bk
nmftgklbpj9onevbzzeightncszqgpl
154fnthzxccjxsztjzpvzcn4
three6fivefoursixgtzfzbkhmnplfm
63eightsixgdsdqqxzzsbnkt782
twovbntmfffivengfbkhzgm4
23sd6nnspq
nineeighttworhtvxdtxp8twoneh
four3threeonehbfhttgn39sqpctngqmzkhttn
rphtbkncs4nznsix
6three2sixsix9eightfour
"""

    //In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
    [<Theory>]
    [<InlineData(1, 12)>]
    [<InlineData(2, 38)>]
    [<InlineData(3, 15)>]
    [<InlineData(4, 77)>]
    let ``2023 - Day 01 - part 1 - example - calibration values`` (lineNo, expectedCalibrationValue) =
        exampleInput
        |> splitToTrimmedLines
        |> Seq.skip (lineNo - 1)
        |> Seq.head
        |> Calibration.valueForLine
        |> should equal expectedCalibrationValue

    [<Fact>]
    let ``2023 - Day 01 - part 1 - example`` () =
        exampleInput
        |> Calibration.valueForInput
        |> should equal 142

    [<Fact>]
    let ``2022 - Day 01 - part 1`` () =
        puzzleInput
        |> Calibration.valueForInput
        |> should equal 55538
 
    [<Theory>]
    [<InlineData("two1nine", "2wo19ine", 29)>]
    [<InlineData("eightwothree", "8igh2wo3hree", 83)>]
    [<InlineData("abcone2threexyz", "abc1ne23hreexyz", 13)>]
    [<InlineData("xtwone3four", "x2w1ne34our", 24)>]
    [<InlineData("4nineeightseven2", "49ine8ight7even2", 42)>]
    [<InlineData("zoneight234", "z1n8ight234", 14)>]
    [<InlineData("7pqrstsixteen", "7pqrst6ixteen", 76)>]
    let ``2023 - Day 01 - part 2 - preprocess`` (line, expectedPreprocess, expectedValue) =
        line
        |> Calibration.preProcessLine2
        |> should equal expectedPreprocess
        
        line
        |> Calibration.valueForLine2
        |> should equal expectedValue
    
    [<Fact>]
    let ``2023 - Day 01 - part 2 - example`` () =
        exampleInput2
        |> Calibration.valueForInput2
        |> should equal 281
        
    [<Fact>]
    let ``2022 - Day 01 - part 2`` () =
        puzzleInput
        |> Calibration.valueForInput2
        |> should equal 54875
 