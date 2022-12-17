namespace AdventOfCode

module Day08 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.TreetopTreeHouse
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
30373
25512
65332
33549
35390
"""

    let puzzleInput =
        """
200111110420341122211300354132530435302054046335421000636521353401523010501311544011230232023001001
001123300003121013211234033001540045254316444345422662334204234115552314354114453103330103243221310
311210241230024303554001043402121246540040235623146113624001464225403350221302241333122301333010300
313220143432400213441134141433022322415401210036504124443562330160631024040400414203001430031231101
102330243131202000523053445134061462231462416310524654563503003141265120040414555415314404230001123
120200202004411420101420552340215506005336303136422632601535115644224525250413000311351122114312432
131000242441124343143442220121045533614100253435644222021120410603556025543143515423332101020113330
114143224444504422040534215205260150032661222324274446404136221346205346041403241445104514040000010
102002200303450412051342412504002161302036524736531157713153330320332655026451125443033542000233234
131431114001235145242516443631640262121512724725312722673711275536055600023502620435102352041434220
100103022300350342104646025232006061267534246117426324115333516241440556345546503014122330134234110
340411310240444523565065200110157265743372741756626771113266576633442122512224300160301523121003401
240201122143332301611525622501152344344127622547454544352463117134635174602401105546324313150204443
414213341225525346306044116014716132415253727642747616421775167655431342165260302156044441001402430
002034014035221165414505053736346311675244143325266652563253566241725376335655642126331133232321323
330402222250004614054205216275746641756514464628248366268327256561454575716451051315302005043043021
241310325523404162223621156275536771142277286437244764364685285652716135255655563443556615122523002
311344014133112343140551161676172477647345586344655542854476838575662251732322131650641333150134303
313204020014065654616432571236166325852282227757483642237635463636324211151244145002606455400140550
242504454234154064241534356714334558678643435742457284425845228775656536216646353660240463251544024
443255334255345102515642652771553735236765262276555262874755434768886657534317475132326043545341141
313340315304653501345423666728588363525342822672368587228672235886573526112475765132626025153524034
051420006655236342265366227126326236258647455994767983899246273464688784734316644433154416143243223
200031324665003061355432372247754556734459659693889684996833862637535475837412657575622420201123423
140344241263650027153125427622833863849658679895838493795964539443535868245563657741334465661643134
325512132604225442275223673673888834666934785658479864956336789437524467868365516754264014325413543
440553621345566755743125253764256458436998973335486489596678374739527657443635661463535511401504210
331212462023141647154212388866373888478938966656994748484744753863394658385867472546571363202432335
354035002310276332316743644554745848468838783939846448697573799767883267528526526111474415445422413
030142301526473113737838576485566368943599475757858476784754765585774388874738823132536606531224033
254504314602665477773248247855977456967998845467559696746769558476557596823546357624134532360044201
242350600526564676764543323725453843834588559589768569964469437679894593323787845373562441364020320
540625010437724231755683634453487863548768644997884799678964549755699554727236477762577133463255344
451252216057515152428338248458343557697558676897477687887944987433654375847562753534376314061014453
330122160467147567486255529944583939789489896889995887484468998693995593385623482244246713633134060
041131501662457631348264888773533498694955585885847947894569788989449795468464743435567116201512356
324606644541656247745643375748597955955965484686798665899998748498763784688345223367323275741212105
253622204212432616625848433733353645955874595667977865588774778887986657388938262376614132365303123
543622014122523657388266376638879877876456588569896878858685887595976387348697782688362257654654456
545255666722761683373236733987395568849985966797858596665995456797686936387467436688227173172634650
064136255231373327225334353677686575877587796966576788998686945884958584564464564576445675644323433
120610001513656435686554865596975695686866776898979998777679686667669834697598378628276111531121113
432140443454112343372324773494755866888666959977967888596565657897466584853384872528674213345455064
535024226551243635756643794753864467875586759796976695856766799459788458859468667424672512313064024
400546563413117325322296559436895867555887966677978878955985988687849746378544845333855566561166301
020506651236764225537855784694794599877755775866666677697878866976479559899456962788386432645006305
443434675663246532365545958356475947658757867798988679678788855744848479896384525363632731537735555
113416556757161662457368859354889664596788879987976669776968868957496979477339774437365431336622525
345262651421638862768869569444995688797756856798788977889589865586877745735587448645533333432761556
123343647632543762526285589568479986597999697766988889879766599964485784375749582642478161171765010
520556433274324287585335433455445745976579977886889677898978799788589696343446976736744523426145632
603056316676658283453298956947885864957677787866666697878756865594484788984693888857823533527626435
303020361354762364376856453974577559959955778797776768878555985959857546898685333344326612443105224
655662135371646362474794695795565787796867968989769897797795799668878548649635844554655635214700534
223523063153272732345883664689487767789978958789978667977668776779785579997898454742636172136404463
352266121152314423833664979937564574668875976796697888965889657968646986463675825327884122351664534
400665311234531444872273846753747848557559659789669976868577568946445475467557535273643277351315425
413615133141512333834886383733778585757577567869576857576866765746564896457868377676563257675463313
206441432112417826772776467473886477685669676669989577999985786548574764376485544784536241352404012
122021466657424225744749755455669764648775997976878875975779856494566487964533526673244445572525445
415624362761157787366445748554448574967467678768669699896677564554676734988937772467213124272261663
333643022543552455773476647846755685745848768897775968859876755557696565349775674843755623245036254
221606155374323788248655473936534859467965767789868557789776666648975994868375432332432722611116020
515203142657661616774357284756583896769987748977678986585896756468657373376756753878242443624411230
402451412661646324875352578545454487959549658876989876657885899454479755887954626826273275501422163
126501412076161365287654767883436597888855968948787589685669794958473484344427835731714123406061554
224552366447424162477676784654557565858779568795449467776596567673499374978454253537362765515136455
005035662123724122426544432497494936376566646685865455484874959734758735857644773573344622333033203
250300234333326527442853884889699384538578958586844694669957899638784976368446656657547266416620501
405422462163345612313725585634533598684989695885578689865948867377943579267254334421525743251536251
402560343153676444275836464535434688496585488668868449558989767733766366582433667223231160634422000
131411565232536316472826883676449835849799446975767985944584684759383758884457627761235345625550522
041123505434326774464264753647664555446469856348997446634837449679564772776833635725271315103402503
401201202325145164315332825286666583564788544463678773777568659553555845288588174227715634160635214
233522655455352676323433643466864478337685895578774998934663536649735254244383133377713110561565534
500524110545525161615415254786767656869878538468666856564943337832285622356527756325471553066535253
405033114656065173147163244736245853758355736973364399758844669463378725783664272727244261206042005
133434313305213626263262454344736258274694568999443935846944973867565366331721727573260654114423513
013203345630556661241544626478668283844274868977767759678387342382675332822247514156535246223022412
141002355104611615351775313274284253255342736736463672767763635857872743177444571552464015412003232
050541022141562500116117245131654655427575824666372574376752344486645526276445751140134353210431055
140102303103536606553371524144146558377427827886845358672865278587236165676573726513622165525303155
424104244245243466654353146535251587878487633252627722887726556783436224443345620155325066433041104
002455502042061044335135432655561337382265222373662265643584526631675372325537502226636204141230114
412131552040462064152362671255772676376836668588286577724785583234517635131726330215411342524535142
033134042522352546543601221312462561135754676576235856725851335243555572652341614000021205122040313
112404022155034452521402331144233444256665441631346243557321561272641474421133343226521330314104122
110044335305055025054363436272234521433267445623673675277464262546643767501655002350651455530223430
441333244524122315641522325466376244237111251433724132655534126562615423051522062643222003544200233
321433203450511332565432323502601421276724652356622771641747753221132040625226510330541220304331102
202120344242300320444020526660612335671161125765246653375477124611533650264413533342122552110001210
432044333332350052202541434520632643263167257637246215627745751715525661105133214051312032214021211
130341104443343550111214142266306461312414544732577364464533134502243632345564040004345330200023433
012444223312111223343044662064252322056623222274577747143503036156466665232254352312552233440423334
111203342423121055233555335666152053641254351422044651426162011166030003364314223001254231442232342
112111241131444014044141505223526650511654461416132520260521415306240142233023200144245200332003400
201333024211020154323024121224536333620215416434241143055243546513406655343432221454411202141430331
302201134433120410222233512424240422022146032412300446653266600261032310032100210333040211333040002
202312021141114142554431112520342120400556304244541024562551661211241403030453530144444200321002011
"""

    [<Theory>]
    // edges
    [<InlineData(0,0,true)>]
    [<InlineData(0,1,true)>]
    [<InlineData(0,2,true)>]
    [<InlineData(0,3,true)>]
    [<InlineData(0,4,true)>]
    [<InlineData(1,0,true)>]
    [<InlineData(2,0,true)>]
    [<InlineData(3,0,true)>]
    [<InlineData(4,0,true)>]
    [<InlineData(1,4,true)>]
    [<InlineData(2,4,true)>]
    [<InlineData(3,4,true)>]
    [<InlineData(4,2,true)>]
    [<InlineData(4,3,true)>]
    [<InlineData(4,4,true)>]
    // interior
    [<InlineData(1,1,true)>] // The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
    [<InlineData(1,2,true)>] // The top-middle 5 is visible from the top and right.
    [<InlineData(1,3,false)>] // The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge.
    [<InlineData(2,1,true)>] // The left-middle 5 is visible, but only from the right.
    [<InlineData(2,2,false)>] // The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
    [<InlineData(2,3,true)>] // The right-middle 3 is visible from the right.
    [<InlineData(3,1,false)>]
    [<InlineData(3,2,true)>] // In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
    [<InlineData(3,3,false)>] 
    let ``2022 - Day 08 - part 1 - isVisibleFromEdge`` (x,y,expected) =
        // printfn "%A" exampleInput
        
        let grid = exampleInput |> toGrid
        
        grid.visibleFromEdge { x = x; y = y; } 
        |> should equal expected
        
    [<Fact>]
    let ``2022 - Day 08 - part 1 - example`` () =
        exampleInput
        |> part1_howManyTreesAreVisibleOutsideTheGrid
        |> should equal 21

    // [<Fact>]
    let ``2022 - Day 08 - part 1`` () =
        puzzleInput
        |> part1_howManyTreesAreVisibleOutsideTheGrid
        |> printfn "2022 - Day 08 - Part 1: %A"

    [<Fact>]
    let ``2022 - Day 08 - part 2 - example`` () =
        exampleInput
        |> part2_whatIsTheHighestScenicScorePossibleForAnyTree
        |> should equal 8

    // [<Fact>]
    let ``2022 - Day 08 - part 2`` () =
        puzzleInput
        |> part2_whatIsTheHighestScenicScorePossibleForAnyTree
        |> printfn "2022 - Day 08 - Part 2: %A"
