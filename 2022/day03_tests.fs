namespace AdventOfCode

module Day03 =

    open FSharp.Data.UnitSystems.SI.UnitNames
    open AdventOfCode.Input
    open AdventOfCode.RucksackReorganization
    open Xunit
    open FsUnit.Xunit

    let exampleInput =
        """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""

    let puzzleInput =
        """
ZNNvFWHqLNPZHHqPTHHnTGBhrrpjvmwfMmpfpjBjwpmw
sbdzQgzgssgbglRtmjlwhjBlfrSrMt
zgsCRzJbsdRVQCDbcgLGWWLnZNGVLLZMNZnq
tvHhRtZGMvMHvfsrBBCTRbwbccRc
qznnlpzzDppWlDpQpCrcrwnBNwTZnBTZrn
PdVZJJqVZdllDPFtMjMgLjGMHvSgMF
csbhhVDDvzlVDcbccGGvfRjDHCjNLRHRCLfmnZfR
dFrStSTTmrrrHVfV
MMgQMMTMVTdgWtwTPwSgWSgGbbppJzlplvhBlPbzhlhbzG
FDJSTtSGhpPFDmFTZDpTFPmCBBrHqsCBhgBlqqrqrlRrHH
dQwMtfdzVwWfwctwnfnQCHllzRrsNzrrgNlCgqsr
fLfQnVjfwQfMdfvfnVvWDvtJPFGDpvZGbZpmbSPP
TzzCrJcDrTDdLDCJDvGNPCFqlZWlvNvWpq
RRHfjsQBFsjgjBQsWqGpNvZQqQlPPQPN
VnHBnRVssnnjsSfBwbMSrrbTwJTcwSDF
HJCgHCCFFFVGJWTlbqDdlqTDDpgl
cZccSmLrfZcrmmzSQftdpDtTHdbQTDMQ
NZZccrrBwZRPNNzmcLSSjJhGhVWCnsFnHBjGChsJ
qwwwJHTHqdFDtZBFPfFBZFzM
gVRcLnnWVgggnnnQgVWWNZtZrBfLBzZzBrMPPrZvPv
GQgQSVRtsVnNRGSCdpmwspmbmDpHmhwd
bhNgNfgwpbLMhCZMGQBmDm
FrcHrSllcqcFFMGLBDQlMDTGlT
FVSddRSJRjLwbjJPJw
wzhhrTwwTrSsdHQjjSHnBjQj
gRDCmVgRgMvtMfVMRBBBhWCHQQHGJHZJQZ
NtgVgttVbMNmvsNlpcrLhLTNPw
MCgjsfnscgjjgnGgJHHqHDgdHbGr
QSSmRFPpRtPFQLQRmPzvBzzzDWqrqWWHJGGNrJJbdtVWHJDV
BdSFdLQzRFlSLmQplffwncfscChhcsMj
GfVmfnmJVnNVFhnhGmbmhpHvqjrzHZBjfvrtBHHZrwBt
ddWQldlMdWMlQsLWTLQgMNwBrvjrZjNrwzZjswHqrv
QQdTRcgTRPDlMQlQPQdhcNNnbJmbGpVnGchFmm
CjjZCCZfvWZRHHhRtwhvPN
mrnqlqMqBlSSLnBTLBwmHPPWhPPHtFRPWzwt
rBVTrrMMSMLQBrndGcddWQbbdZfCZJ
LFtdjHjLjLqHqstLTjFLFqNMnMhhZdDDNMVbWdDDbhnZ
CrBpBGnzrzmczcllrphCZZWJMDWRbbZNMDMR
GwgvzpzvrcmBrnfHjTgqTsgHjF
rMPPZcplCZlZPwtSwhtBwCQQzB
FvDGffLqqmQFwmmhzt
TjJjJfHHVDVnHVgZZlQppcVscP
hVcqHwhgwwwjHjjGWbvrbBGrsWVWGn
CttPRpMmPDTWbWltlLBnGl
pZmDFMmPMfnZwqqwfcqJdHgz
bSJWhWJCbGGWJPStWTgRQwzDjgQQjsDW
nFBBVQVrVBrNFMFZVpBBZFZrDgdTldgsRsslsljsRzTRjzns
rMcZcHcBQPvbbHGP
mSfmwqfmzrfHwFfmrwvPHqPmMFRlMDDZBCVVRCVZVlZpMRRR
TWjdTWhTsssLTGsJNWhTQddjRMDMtNNBSCDBllMMBVtDMVRZ
QhWTQcdhjThsdGbTLGjWHmffnmHwnwHrwqmmfcwS
LmrsMQnnpfmMLllvTvqvFFzvFHNN
WGRFVWdwZWZvCbJzcvJNzw
VjGhDtWGSFRGjVVSFdjjDPBfspPnnMBLPLrrpMMm
qqqCCJjtqtqCtqLZspHWBdSrWWSzzbzHFWBldb
GhwwcwPFVDcNFRRGwwzmlBrBWvllrvSzlrcd
DGGhQNNDhTpZZqqLQFQQ
QfZmgQQZCCMLfNrgprdNvvdrTg
hhttsBmBDcFRBlJshJcRrnjnTvNqpddNNqvndp
JtsGJGtGGJJJHDbctllhZHmMwMQSPVPzHSLMPZmV
DScSjZcNBZqjDDcLLfFtPfCfjfPvfv
pTmRlWhdMwTLGwCf
mRdWCVVglWrCmVHVrVCmdrbSzNcBDBqBZDNHqssscNzqNc
sPMHGFMsrPNCPnNS
ffJzllbzpZBllttBtfglgBTbSCVCmmrNFmmbFNvCFLLb
cpZqpfgZZJtJqJJJfWHWhHdHWHjcdRdFHD
ZZPfppvzMrlNBFcvFB
shJgstJwWLVJwcrFFVFrBVNNqFFB
HwWJdLHWWLcQgssHwwSQSQtQzCnZZMpZCmdzZCzpPzpCPRCj
QCpLRbsCCQQLbQzCBQDQBBfTTffWtTctJVRNVtnfwtWV
GvlqqlGlmMrdsvrhmlcTvwJtwNwTvfJfcWTW
lMhgqGhddjqFFCzBBpbsSQpD
JJwGJwVQQwVSsSMhQMQgHfgfTtrrfVTNgNNfrt
dFDWCDdFppvDFmWWWnJTPllHmHlgrqrgggtH
DzFbWjdRpbdFCjjRbnFbQBGhhQBBJZwMhScwZwJz
HttvHpHmpJWtHmFNvlvdMSVdPMtLVCCMMMfcfL
GjgzhGSGSSdCcRMVjMdc
QshbnghgnGDnqsFrNSJFrsNs
wJpjMwzjzdVbzPPVpbCHnqGnBqnsBrNCwgrC
ftTLLDTQtLTGTGtFrgHrvqgQnrvQsCHH
fTcFFfLSfFFcGFllcFhPJPjWWJSjSWzMWPdS
ZjNdmjVQVZmvNNZNNZHWZmWtsJnwTpJJswpWwGqJhJqGpp
FcRRcDblDMLRcRMLFFMDGsJnqhwpqTTJGwnsfnlp
LRBrcLbbgLFgBbFqDvdHQvCCjNzzzVrZdV
BdbLWrgdvgWvVJgWnDfNhVnqhCCpDpcq
tSQPSTSGPMmlMPtQQPJGtGQRCcnqqfnRhCcChDqnCfRScf
jTssPsjMQMmszPjlTtsJdFBFrJzrbJdHZFHdWH
vCccctvvTTtZcgLGcZTbssbMWnpMpmLWqnNjpfPPfPjMPp
wwBBlRBBwDDVFRhFlRhdRRVWPnnpMpffmmffrpWqVNPm
ddhddRzHlQHFJcGsCztTgbNzST
fJctfpVWcnfRLfrRwP
vmmnvDQDZTNTmGGTqTMTvMqwBdLjBvRzBRrRBRLjjBPzBB
GMnmqSTFFQqttcbcJWgsSt
rHNfmfRsmfRGfDNcRmcmMQlLCGSnQwwPPCSnzQlSCl
bsJTBsVhFsVpqFWFgPCwnQwBZzwQzZLlzn
qggTTqvqgqbbTTFqVqgWqvNmmMMRdffftNfMDMmscR
rFWQFszrwjsjFWvshPTCmLZLSTLwSLlgSP
BQbcqVHNVqVpVpmClJgJJHSmZLJm
qBNNNVdDMGBpDcDWsvdQsFrFnjttfj
qGhmttmzhtMvhbrLdSHbdSHRzb
WCBgQJJpjCQlgdHZrfPRPSRbNg
jBTTDjlnjnJDJTQCVntcwtwMSvqcGFDhcvsh
ZTrnTqMWWWnfrddMGJPgPLlPbw
VvmGRVpBpNNmvNvjVjtpNpCNLLLJHHBdgLPdwsdsbLlwwlwb
GmCVSCRVGmpCRVvttmpDrQZfhnzhzqnDWnrZZTQq
DQBZHHtWHzSvZvDQWchgqsqqhrrhhcqrcZ
jdMfwlFfFlTfndwpjjwGnNrqhPTmPSPTPPhmgrPSrh
jlGbwGMdlnJpGFGjpnFCSJzzDDtWHCBBQBvtVC
RrbBWBRRWSRsBBVvsPHZDwSjjPdnHwtPtH
fTgfzMmNJpmJgfllgpjVQtDDndVQpdnHVtPp
gGmlNclTGmGFhLVcVrvLqrvc
QcpCTVCZVcCwLcCVvHvvVsCcNzNNSbPRzsDRDSBlsNNzDRtb
fggMfJqgrWFpmjWMggmrfMWNSbRSPBDbNtJRtPJzlStBbN
gdnmpWGnZvdQCvdv
tqqcLqqDDqNtDrqHrrPWlTlTWZTMzTFzQlMPSZ
pfnpmmppmppRGjwbjmnjwspWbQQQTMWZbCTSZCSQlCllZF
gmpVnGmmmpjDvVLBFqqvrH
LqBvJHZvbHGBHrBtGGQTmSVprVzhpVPDPQzQ
CRdRgwCfhTVDzSdQ
fRCcjgSMjfNgMMLGbGZtvBbGHv
HgvtDDzDpvwgvvqdHPZWdMssTTddSs
rJFrGNFVQmNFVmRnWhhsrTbhwhZTrdTd
VQGBBBVNQClpcBvBwD
PWlSzZGmdmGmlGmhggBpvMjvMjFgPJ
TtLRDtQQfTVcQQQRtBsJFFccFjWhJJFMBs
HqVCNtWHCDwdnlGwGqSr
RwdRJgCJRGGmdMbcGbdnTnTtttLLnptMtMtMqZ
DWsWPFrPqVPPLVCB
zQWWsslsQHFhDSszDSFQzJJJmvcgblRgmNvCJmvNgw
tpmFrWTtRpRTtggsSlnQpsnnlSHPsn
bZwZjNNZGLSrVsGndPPV
NvrcjCfbvvLBDBWfWFgRRm
WWFMgWmMhhwDcMMMDcmLWLtQwwsjbsQHvZHbRjZfsZzH
PTCplTCdSJJCpvPGNSvsbsfHtbQZzdHjQtjjsj
vNGJPpqJvJvqghgFgWFmLD
RlRpLTZCjWRjRWwpRsjHjbSbqMqMvvnbnGMnGGqQCq
gddfDNczmgPthNcDdgPVnbbzbnJrJJGSSVJJQS
BmDmcDmcmhffdBHlRwjRLpwlWQ
prQlfzlWRPzgQWzlMPMRppssHHsDsHjwnHHbWDwwbwjL
vFBJJtZNShJvZFtdSqtmqjTDVHVGDHbwVHDVsDnThH
vcjBZZdZqvCfpzRfcgRp
cggpqgRlSpNsgNggbjjj
ZZSSJVLVLFDZWNGjCWWbCjsF
vZLvfZQQfQtJVJDQShLrLfMmnldmwqwTqqMcMTMTndrm
bQBMtBPddtMFbJFhRGzMfzvnRGRSvWnW
TmHTqlVHwVpQqjmwGvSgSpnLpzfWGWSn
TTrDQCDrrTmDCCCVHHQZBdZFPdsNdFBtFDhtFB
fjpQvNZcGhGGTtQS
DVJzvbVmHbbtSTSTRStzTM
VDvmqllmJfjWlnplNs
ZmdHZJjvQLdRjpmLJrqqZBhhtCschPfBPcrDfPffCD
MWWSMMwnwlSgzWFFgSwzVwzqcfDCfChCbbtssbfDChcD
NMqFTwGqMwgwwgjHRdHRjdmQmQTm
TTqWPCWRhTWqPNjPJMNtrlbJFttQwwrBrlbwlc
GfpSDGZvpQffSHDgggDZrHctFmrHncnnwwbBtBrt
SQGfLsSLZsqMTRNMPT
HdBdnBZJTZBBmsfwwBlh
MjCVjzwqWrfzplzW
vVbqCjjRgjwMbnbGHJScScZHLL
dwwwtCdznvDDFrMrrw
GmWLQmgQmHgcdGcsTgTDqDbSfFWfMDMfbSNqvr
QhTLmVQHLmdLTjGGVptRnZpZBZVRpPpP
CzjFpzRHdtBFBCqNqSbJZWcQJTSbQjMTWZ
wGwVLlGrdVGwDnwsgfMSZvJMbWJcWlvbbMSc
rDfsgggrGnGngsPwdVLfDnmDtzzFNCPHtzCtFHpBRqhPztzR
mrgWzBcDtVCcQcCCdscf
LRJhjRjPZvqSRGhGjLgMCdHpMNwQCpMHpHMS
GRvGJRJjqPZbvGGhRjnqLJWtgFgtzTzDrFnTWrlTlllW
cbmcddlffvbTfvFflpZzsMVNznNVlnqnzqHMNM
StWJBQRWLRWNPNMCswRVHC
BJQBhSWhjSthJQGGWWggJDDDfbdbbfHbddbrFrddvFvv
jFqvqvWZWDtBJrrlrq
TzGcbHcrmVzMGNSmTcGDtBthJCNtsJDlBCghgP
bTrnTccnLSrrTHbnwfLjfdvRRwZFdwfR
drHVrdVDfsDbVsdVDbVqRwbZZwCRCCCJlJThwRgT
jFPcFpBSvtNPzSFcjcQpcQjpThZCRltGRRRJhwCwGhwgwhRm
SQSzPBjjPPSvLqqssdnqLZLMsM
bQTWlWlvQclNwwWlCCLStCRSSjStpj
zVZZDdBnBmgzVsjsLthSpshdCL
DfBnrmBmgzHBfDHmnGrNFCwQvTPvqCTwqTFGbF
srSWJnrbmlWlbhzsWszSvPGwvgDhcjdjjfvhjvGv
BRRQFLtNfQNMpqpQHDjdDjDcZZcvwZZHPH
NLCNCtRQfRttRFRCTqMBqQQrzrbzrlJmVVbsSWmVrTbSzJ
RHLfLcSRTFSghLRHGbwZmMZddgJswZsbMm
ptqjtCzzQztqCjDlBGpDpbMZdwmMbZsdwNmdJpbs
tttzCVllDCtDQnQBVHGHWvWTLWcLSLHf
FVlNnPqbGTHftghggJqf
zLcZWZpWWrcrZLLZDWrwMcrhBFBttChBmBgptChhtFftmf
LZZLrDrrDDMrcwrDwsWFzdTlnGQPQQVbdbnsvnvsVQ
BbPNMJNbQvDbvPLwHflczlwwzf
pZjWZGZjFGdgpnVgZhghdmcflrlswzzcstlrLwhtwc
WZSdqFjqSqSWdGFjZpdMTTDNTvLCRRLLqRQMCN
FqgFGtbgTvRwrLqhvw
JCCWJWCdJMQNNsSWsMPQRDDLDSDLwTrrvnwfDvnD
HdPJlBBHCCQdBMWdTtVbgHczGVGjmtzG
PLlZDLZDsFCvbDQv
HVcTmVmJqVzqczfzbjvvCFMRfCsWjMvR
cqHzTqJTTTTzzmnmrctrBlLlvSlgLdZvSwSlpw
SbMMNJjmgMnJdSSbjVFZVSQrlQfWVQVWZh
PtqDqPGcLHzHpqLcRzRsfQFfZlfRfZfRFVsl
cTDLcqGCzDTqzzDLDzqPTtJvbBJMnmvjbdlmJNvmdgNC
tDJDlZVqJGbvHNQbNFFsFPmLns
ppczpzpffGwfBNLGmn
WShzgTTpWzhWztJJGJSvtvvtjq
TbZFTFScnCZFQRTCqQdBjdJqjBqjjQDB
rmmLpLLfzrlmslMBHvdRddNDDJDrqD
MWwLPzmWfpsMmmlMPMWLwRTZTZnnTcVCcZFCwSnZ
SqmClqHssNWCqPTcWcGhBTchVV
ZnnnDflRpBVTTVhPBZ
DpgfvnvMfCsqlMtSll
ZzLMRZpLMwwppZqnQGvQgBSvlNVlBFFNFVrg
HcqhTmhmdDTPFTJgTTFBSgJN
mccPdDDHbssbtwZMqpbzCRGM
TgqnTltgWqLRSRnlqddngFfrvHvrBTfCCFrFVTvVCf
cwNJmPzQwNzczzNsJGhhHfhrfvVHGvtvVVfC
jjtbtDswcmPWlbgRnRdMZL
TmpTBBwvspTptRmsmTGLQDGRHGgVGLSQSMHQ
ZlPWqjWrzjPqdrlzbrbrwfrWLHVMLnHDMVDQnLQfQfVngQLS
zNwbrrFWbFJpmpmvvt
RMQQMwHMMzcFsWsDrWfcpJpS
LLhZmGVLhVlTZfWWfWpCrDsGSp
VLVTnqjjZngtQRFjvzDM
gmRBpjrpRvCfRCrBgvjHShnbnngbgSJnNsHMHS
ZDPTwGWtqwHhSnbcMNJw
DWGGqtVVqldWZzMzWmvjrjprLRFjRVvvff
tCzVzsVtDFzssnSsgdqJdCNqJhmgmpqq
PZccPGvQfRLMQwNdhpwhNh
jLrcbRjPZBrcPdjRHFlWnVtBFslSWznW
vvvbJbWrLvFWHzZzZRhB
chtwTmCNlRRZzRPT
hmcCssCswrMDGMSrsr
LStGBsQLlllhzMzs
dzVZDNWRDdZNDTZTPvWVhhphpMlfMccRmfnlMlRn
VFvgTrNPdFWNNFNFTzTFFSjSQBCqrtQwSBGLLBGwGL
qGJSJhWStdSfWvSvtGRRnzRDDggrgvnzsmRP
lTTLpcljjGlLlLNBpjwFQDQmRnrRDPrPscRrDDng
NCNjFlHNCTVjpwGqGSVbJddqZZJM
MbWdgvHFlMvmzTzShvmm
tqjqpLsNsrrsjstNLpQrGVhVBzrhVcfmchDcTPVVmc
RqwjqjqsGjjGGQNjGpQZpqRFJgmMHwdbFWgnHMFdwmmCFW
HHHLcCcVHjTHglsB
wDSRwzzRpMSdNSPSwSpRbqvgBsdqlgTvBFBjgFvvgB
RpbzPssDMWwNRbRNRPDsDhJthLQVGLJcctQCJQfQJCLm
WsZgbNgZVCCWbVVVmgZbCCRPccGnzPBqJjzWJBJPzvBvGz
SpfThHtrHFBPPzJvPntj
QHDhhrhpTQpHhQHnfwnTCNlbZCCDLNllZlVsNCNl
QtzJFRQLMRnZcZsfcphlPQ
qSBbjmWSCNmVldSqqSqmjCSZshfwfrPPZZfcPVZfhgsgPg
HqBbHqBGSlNBbltnLLHFJMtRvRTD
tcGtDdMcttttHNBlMctldlwjwwqqCLCwDwZjFCZhmnwC
VrJgvWWsPvRgVgrJQvfQfzgVzZwCbLZmnmwCwZqmnhjZbnLj
sJpffsRWWRJVWWpHltSpnMHGcMTl
zNqRbqSbfdcTLLfS
ZVPzPnVvdLwLDPfF
VWnzQCVWZVMzQRHgqgqrHGtGMp
PbHpWfWPvRfbzWPFfRpPDtBwSHMwCBgDwBjDtMMM
hTTdZQlcnTcmqVTdcddrDgBSwsjjBgqBtsCgMD
hlldTmdJJmJdZvzfFfNJFJgRzR
PJWvJBbWsfLQWsLvmCqHCcNLHqHLLcwDqV
dQztrZrdwHhptqDH
ZrMGjgMSrdzQGQRJPvGGbm
RmjljZChlDZBCRRvlmNSLSqMNLzwLvppwQSQ
sTnVnPrVGsGTPddJrfgQgqLgGpMNQtgNtNzg
sbbTfTdcJPnHbsJfHsdcmDDmmqBZlClmjBRDCZ
CJmHLmHFFCFbHsbJsJqvqhQqLDhQZvnQDZnn
wGwppTjdWPdgFpGcScBqNnNqNhQlDqnDlZZW
pGcgGgTpGjFdwpSFVgSdpPjrMCMffzJzRzztRfHCRsVmtbsz
CgBClZfCflPflNZRvfQswwmwmwQsQhgppdhm
qbzDGrjLLNLDHDqtJmmhhmQdhwpQhhbp
NLGqVqjDjjGrMFrvFWPBRBZnCvfFnT
tbrrHsgsVmmmbtgwVsQRqjJMmqMjQfJfLFLD
ZvlBGzdvjGfRFJQJ
dBppnnBBhdzZncBPlznpnNdWHSsbWthbSCgHrVfgSSwVgr
VRvMtRVFHQLvMRQFQtBctrthshTTgCmhTrgWhWZsZZ
lzJlGBSPPhzjgZsTCr
wJlpJPfDSpwBnddqJDdpPpcvMFHFMvNbvnNMFHHRVVbR
CPShbbdlGCdQqlRPGPdlDWDFzjtFjggCDJgWczfF
mrHrTrrBMBsmNsrwsBpnfpggDDcjjDDpjzFJzzjtJz
BvsNvBLHrrrNvwBTNNsNGbdQhlPGGfqhhRGqLGdl
PSSlPtlStGhPNMtwPMPJzDddnbnDNTDDnJqjbz
FFVHRwVLvFvVrVHrZcLmRHggjDmdDnDnznnznzQjzdmJddbn
WrvgRgcRcRrrcRvgcVrHVrwCCSfsCsGsllhMSSSSMttlSCpG
hBPJqVZTqqPSlGlfddfddZvl
JWWMJCpnMrmztzdjnzld
RbWsrwMrpbRspbWgpwhLJPccNVqLLPSVgVPV
hcTrWqcfhwGfWrWMjHjGvDHPmJMDzF
ZtlsnZZtLBSbSssnbndjDJJFHFHJPHPsHMTHHM
ntRZtSbtZgZStTqchwQfRwNpcq
GfLqrsqQGgPgjjQGVcNvTpTpNFcWPvPPpT
bRnRLnMZFdCMcpvT
RnRhzRlmlhhHhhmhRsqLrfzrGVSrGBSGrL
fbMffwdZsncrGcfG
qDBjSSLqhLBSmDbjqNhqTLjCGrCHGrvcGWcpWcrGWnCrpm
STLDqbhTLqNTNSRhlwZlJlRQFFRwMdPQ
TVVGNFggcjPPJzwvQlRRwRvSlcSc
frsBbWhtSRzSLfRf
qDCqddbsWrqzhsdNmdJNJHjTggFFVV
NTWTDrSdFTLtPTGf
lZqjHlVRvRltLtRWFMtFLL
qvjWzzvVbZpjqllggscdchwDrCphwsdhrD
"""

    [<Fact>]
    let ``2022 - Day 03 - part 1 - expectedContents`` () =
        let expectedContents =
            [|
                ("vJrwpWtwJgWr","hcsFMMfFFhFp");
                ("jqHRNqRjqzjGDLGL","rsFMfFZSrLrFZsSL");
                ("PmmdzqPrV","vPwwTWBwg")
            |]
        exampleInput
        |> toRucksacks
        |> Array.map (fun r -> r.compartments)
        |> Array.take 3
        |> Array.map (fun cc -> (cc |> fst |> System.String, cc |> snd |> System.String))
        |> should equal expectedContents
        
    [<Fact>]
    let ``2022 - Day 03 - part 1 - itemsInBoth`` () =
        let expectedContents =
            [|
                "p";
                "L";
                "P";
                "v";
                "t";
                "s";
            |]
        exampleInput
        |> toRucksacks
        |> Array.map (fun r -> r.itemsInBoth |> Array.ofSeq |> System.String)
        |> should equal expectedContents

    [<Theory>]
    [<InlineData('a', 1)>]
    [<InlineData('z', 26)>]
    [<InlineData('A', 27)>]
    [<InlineData('Z', 52)>]
    let ``2022 - Day 03 - part 1 - asPriority`` (c: char, p: int) =
        c |> asPriority |> should equal p

    [<Fact>]
    let ``2022 - Day 03 - part 1 - itemsInBoth - priority`` () =
        let expectedContents =
            [|
                16; // "p";
                38; // "L";
                42; // "P";
                22; // "v";
                20; // "t";
                19; // "s";
            |]
        exampleInput
        |> toRucksacks
        |> Array.map (fun r -> r.itemsInBoth |> Array.ofSeq |> Array.head |> asPriority)
        |> should equal expectedContents

    // [<Fact>]
    let ``2022 - Day 03 - part 1 - example`` () =
        puzzleInput
        |> part1_find_the_sum_of_the_priorities_of_the_items_in_both_rucksacks
        |> should equal 157
        
    [<Fact>]
    let ``2022 - Day 03 - part 1`` () =
        puzzleInput
        |> part1_find_the_sum_of_the_priorities_of_the_items_in_both_rucksacks
        |> should equal 7878

    [<Fact>]
    let ``2022 - Day 03 - part 2 - example`` () =
        exampleInput
        |> part2_find_the_sum_of_the_priorities_of_the_badges_for_each_three_elf_group
        |> should equal 70

    [<Fact>]
    let ``2022 - Day 03 - part 2`` () =
        puzzleInput
        |> part2_find_the_sum_of_the_priorities_of_the_badges_for_each_three_elf_group
        |> should equal 2760
