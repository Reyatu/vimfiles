" Mostly copied from ARMTABLE.INC

" Last Change: 2015 February 03

syn keyword fasmarmTodo FIXME NOTE TODO XXX contained

syn keyword fasmarmMacro addr apscall endl endp import literals locals proc

syn case ignore

" Data directives
syn keyword fasmarmDirective db dh du dw rb rd rh rw dd file

" Special operator for data directives
syn keyword fasmarmDirective dup

" Control directives
syn keyword fasmarmDirective
	\ label if end else used defined relativeto times repeat break while
	\ org load from store at virtual align display err assert

" Preprocessor directives
syn keyword fasmarmDirective
	\ include equ restore define fix macro purge postpone struc restruc
	\ rept irp irps irpv match

" Formatter directives
syn keyword fasmarmDirective
	\ format binary as use16 use32 use64 mz heap segment entry stack pe
	\ console gui native efi efiboot efiruntime dll wdm large on pe64
	\ section code data readable writeable writable executable shareable
	\ discardable notpageable export import resource fixups coff ms ms64
	\ linkremove linkinfo extrn public static elf elf64 interpreter
	\ dynamic note

" Undocumented flag for PE format
" http://board.flatassembler.net/topic.php?p=132492#132492
syn keyword fasmarmDirective nx

" FASMARM directives
syn keyword fasmarmDirective
	\ code16 code32 coprocessor processor thumb thumbee dwarf

" fasmarmInstruction {{{1
syn keyword fasmarmInstruction
	\ absd absdm absdp absdz abse absem absep absez abss abssm abssp abssz
	\ acsd acsdm acsdp acsdz acse acsem acsep acsez acss acssm acssp acssz
	\ addw adfd adfdm adfdp adfdz adfe adfem adfep adfez adfs adfsm adfsp
	\ adfsz asnd asndm asndp asndz asne asnem asnep asnez asns asnsm asnsp
	\ asnsz atnd atndm atndp atndz atne atnem atnep atnez atns atnsm atnsp
	\ atnsz bcc bfc bfi bkpt bl bxj cdp cdp2 cfabs32 cfabs64 cfabsd cfabss
	\ cfadd32 cfadd64 cfaddd cfadds cfcmp32 cfcmp64 cfcmpd cfcmps cfcpyd
	\ cfcpys cfcvt32d cfcvt32s cfcvt64d cfcvt64s cfcvtd32 cfcvtds cfcvts32
	\ cfcvtsd cfldr32 cfldr64 cfldrd cfldrs cfmac32 cfmadd32 cfmadda32
	\ cfmsc32 cfmsub32 cfmsuba32 cfmul32 cfmul64 cfmuld cfmuls cfmv32a
	\ cfmv32ah cfmv32al cfmv32am cfmv32sc cfmv64a cfmv64hr cfmv64lr
	\ cfmva32 cfmva64 cfmvah32 cfmval32 cfmvam32 cfmvdhr cfmvdlr cfmvr64h
	\ cfmvr64l cfmvrdh cfmvrdl cfmvrs cfmvsc32 cfmvsr cfneg32 cfneg64
	\ cfnegd cfnegs cfrshl32 cfrshl64 cfsh32 cfsh64 cfstr32 cfstr64 cfstrd
	\ cfstrs cfsub32 cfsub64 cfsubd cfsubs cftruncd32 cftruncs32 chka
	\ clrex clz cmf cmfe cmnp cmpp cnf cnfe cosd cosdm cosdp cosdz cose
	\ cosem cosep cosez coss cossm cossp cossz dbg dmb dsb dvfd dvfdm
	\ dvfdp dvfdz dvfe dvfem dvfep dvfez dvfs dvfsm dvfsp dvfsz enterx
	\ eret expd expdm expdp expdz expe expem expep expez exps expsm expsp
	\ expsz fabsd fabss faddd fadds fcmpd fcmped fcmpes fcmpezd fcmpezs
	\ fcmps fcmpzd fcmpzs fconstd fconsts fcpyd fcpys fcvtds fcvtsd fdivd
	\ fdivs fdvd fdvdm fdvdp fdvdz fdve fdvem fdvep fdvez fdvs fdvsm fdvsp
	\ fdvsz fix fixm fixp fixz fldd fldmd fldmdbd fldmdbs fldmdbx fldmead
	\ fldmeas fldmeax fldmfdd fldmfds fldmfdx fldmiad fldmias fldmiax
	\ fldms fldmx flds fltd fltdm fltdp fltdz flte fltem fltep fltez flts
	\ fltsm fltsp fltsz fmacd fmacs fmdhr fmdlr fmdrr fmld fmldm fmldp
	\ fmldz fmle fmlem fmlep fmlez fmls fmlsm fmlsp fmlsz fmrdh fmrdl
	\ fmrrd fmrrs fmrs fmrx fmscd fmscs fmsr fmsrr fmstat fmuld fmuls fmxr
	\ fnegd fnegs fnmacd fnmacs fnmscd fnmscs fnmuld fnmuls frdd frddm
	\ frddp frddz frde frdem frdep frdez frds frdsm frdsp frdsz fshtod
	\ fshtos fsitod fsitos fsltod fsltos fsqrtd fsqrts fstd fstmd fstmdbd
	\ fstmdbs fstmdbx fstmead fstmeas fstmeax fstmfdd fstmfds fstmfdx
	\ fstmiad fstmias fstmiax fstms fstmx fsts fsubd fsubs ftoshd ftoshs
	\ ftosid ftosis ftosizd ftosizs ftosld ftosls ftouhd ftouhs ftouid
	\ ftouis ftouizd ftouizs ftould ftouls fuhtod fuhtos fuitod fuitos
	\ fultod fultos hb hbl hblp hbp hvc isb it ite itee iteee iteet itet
	\ itete itett itt itte ittee ittet ittt ittte itttt ldc ldc2 ldc2l
	\ ldcl ldfd ldfe ldfp ldfs ldmda ldmdb ldmea ldmed ldmfa ldmib ldrbt
	\ ldrd ldrex ldrexb ldrexd ldrexh ldrht ldrsbt ldrsht ldrt leavex lfm
	\ lfmea lfmfd lgnd lgndm lgndp lgndz lgne lgnem lgnep lgnez lgns lgnsm
	\ lgnsp lgnsz logd logdm logdp logdz loge logem logep logez logs logsm
	\ logsp logsz mar mcr mcr2 mcrr mcrr2 mia miabb miabt miaph miatb
	\ miatt mla mls mnfd mnfdm mnfdp mnfdz mnfe mnfem mnfep mnfez mnfs
	\ mnfsm mnfsp mnfsz movt movw mra mrc mrc2 mrrc mrrc2 mrs msr mufd
	\ mufdm mufdp mufdz mufe mufem mufep mufez mufs mufsm mufsp mufsz mvfd
	\ mvfdm mvfdp mvfdz mvfe mvfem mvfep mvfez mvfs mvfsm mvfsp mvfsz nrmd
	\ nrmdm nrmdp nrmdz nrme nrmem nrmep nrmez nrms nrmsm nrmsp nrmsz orn
	\ pkhbt pkhtb pld pldw pli pold poldm poldp poldz pole polem polep
	\ polez pols polsm polsp polsz powd powdm powdp powdz powe powem powep
	\ powez pows powsm powsp powsz qadd qadd16 qadd8 qaddsubx qasx qdadd
	\ qdsub qsax qsub qsub16 qsub8 qsubaddx rbit rdfd rdfdm rdfdp rdfdz
	\ rdfe rdfem rdfep rdfez rdfs rdfsm rdfsp rdfsz rfc rfe rfeda rfedb
	\ rfeea rfeed rfefa rfefd rfeia rfeib rfs rmfd rmfdm rmfdp rmfdz rmfe
	\ rmfem rmfep rmfez rmfs rmfsm rmfsp rmfsz rndd rnddm rnddp rnddz rnde
	\ rndem rndep rndez rnds rndsm rndsp rndsz rpwd rpwdm rpwdp rpwdz rpwe
	\ rpwem rpwep rpwez rpws rpwsm rpwsp rpwsz rrx rsc rsfd rsfdm rsfdp
	\ rsfdz rsfe rsfem rsfep rsfez rsfs rsfsm rsfsp rsfsz sadd16 sadd8
	\ saddsubx sasx sbfx sdiv sel setend sfm sfmea sfmfd shadd16 shadd8
	\ shaddsubx shasx shsax shsub16 shsub8 shsubaddx sind sindm sindp
	\ sindz sine sinem sinep sinez sins sinsm sinsp sinsz smc smi smlabb
	\ smlabt smlad smladx smlal smlalbb smlalbt smlald smlaldx smlaltb
	\ smlaltt smlatb smlatt smlawb smlawt smlsd smlsdx smlsld smlsldx
	\ smmla smmlar smmls smmlsr smmul smmulr smuad smuadx smulbb smulbt
	\ smull smultb smultt smulwb smulwt smusd smusdx sqtd sqtdm sqtdp
	\ sqtdz sqte sqtem sqtep sqtez sqts sqtsm sqtsp sqtsz srs srsda srsdb
	\ srsea srsed srsfa srsfd srsia srsib ssat ssat16 ssax ssub16 ssub8
	\ ssubaddx stc stc2 stc2l stcl stfd stfe stfp stfs stmda stmea stmed
	\ stmfa stmib strbt strd strex strexb strexd strexh strht strt subw
	\ sufd sufdm sufdp sufdz sufe sufem sufep sufez sufs sufsm sufsp sufsz
	\ swp swpb sxtab sxtab16 sxtah sxtb16 tand tandcb tandch tandcw tandm
	\ tandp tandz tane tanem tanep tanez tans tansm tansp tansz tbb tbcstb
	\ tbcsth tbcstw tbh teq teqp textrcb textrch textrcw textrmsb textrmsh
	\ textrmsw textrmub textrmuh textrmuw tinsrb tinsrh tinsrw tmcr tmcrr
	\ tmia tmiabb tmiabt tmiaph tmiatb tmiatt tmovmskb tmovmskh tmovmskw
	\ tmrc tmrrc torcb torch torcw torvscb torvsch torvscw tstp uadd16
	\ uadd8 uaddsubx uasx ubfx udiv uhadd16 uhadd8 uhaddsubx uhasx uhsax
	\ uhsub16 uhsub8 uhsubaddx umaal umlal umull uqadd16 uqadd8 uqaddsubx
	\ uqasx uqsax uqsub16 uqsub8 uqsubaddx urdd urddm urddp urddz urde
	\ urdem urdep urdez urds urdsm urdsp urdsz usad8 usada8 usat usat16
	\ usax usub16 usub8 usubaddx uxtab uxtab16 uxtah uxtb16 vbif vbit vbsl
	\ veor vmrs vmsr vpop vpush vswp wabsb wabsdiffb wabsdiffh wabsdiffw
	\ wabsh wabsw waccb wacch waccw waddb waddbss waddbus waddh waddhc
	\ waddhss waddhus waddsubhx waddw waddwc waddwss waddwus waligni
	\ walignr0 walignr1 walignr2 walignr3 wand wandn wavg2b wavg2br wavg2h
	\ wavg2hr wavg4 wavg4r wcmpeqb wcmpeqh wcmpeqw wcmpgtsb wcmpgtsh
	\ wcmpgtsw wcmpgtub wcmpgtuh wcmpgtuw wfc wfs wldrb wldrd wldrh wldrw
	\ wmacs wmacsz wmacu wmacuz wmadds wmaddsn wmaddsx wmaddu wmaddun
	\ wmaddux wmaxsb wmaxsh wmaxsw wmaxub wmaxuh wmaxuw wmerge wmiabb
	\ wmiabbn wmiabt wmiabtn wmiatb wmiatbn wmiatt wmiattn wmiawbb
	\ wmiawbbn wmiawbt wmiawbtn wmiawtb wmiawtbn wmiawtt wmiawttn wminsb
	\ wminsh wminsw wminub wminuh wminuw wmov wmulsl wmulsm wmulsmr wmulul
	\ wmulum wmulumr wmulwl wmulwsm wmulwsmr wmulwum wmulwumr wor wpackdss
	\ wpackdus wpackhss wpackhus wpackwss wpackwus wqmiabb wqmiabbn
	\ wqmiabt wqmiabtn wqmiatb wqmiatbn wqmiatt wqmiattn wqmulm wqmulmr
	\ wqmulwm wqmulwmr wrord wrordg wrorh wrorhg wrorw wrorwg wsadb wsadbz
	\ wsadh wsadhz wshufh wslld wslldg wsllh wsllhg wsllw wsllwg wsrad
	\ wsradg wsrah wsrahg wsraw wsrawg wsrld wsrldg wsrlh wsrlhg wsrlw
	\ wsrlwg wstrb wstrd wstrh wstrw wsubaddhx wsubb wsubbss wsubbus wsubh
	\ wsubhss wsubhus wsubw wsubwss wsubwus wunpckehsb wunpckehsh
	\ wunpckehsw wunpckehub wunpckehuh wunpckehuw wunpckelsb wunpckelsh
	\ wunpckelsw wunpckelub wunpckeluh wunpckeluw wunpckihb wunpckihh
	\ wunpckihw wunpckilb wunpckilh wunpckilw wxor wzero mlas orns rrxs
	\ rscs crc32 smlals smulls umlals umulls
" }}}1

" Case insensitive macros
syn keyword fasmarmMacro common forward local reverse

syn keyword fasmarmOperator
	\ bsf bsr eq eqtype in mod not or ptr shl shr xor fit

" Special operators
syn keyword fasmarmOperator plt rva

" fasmarmRegister {{{1
syn keyword fasmarmRegister
	\ a0 a1 a2 a3 a4 apsr apsr_g apsr_nzcvq apsr_nzcvqg basepri
	\ basepri_max c0 c1 c10 c11 c12 c13 c14 c15 c2 c3 c4 c5 c6 c7 c8 c9
	\ control cpsr cpsr_all cpsr_c cpsr_cf cpsr_cfs cpsr_cfsx cpsr_cfx
	\ cpsr_cfxs cpsr_cs cpsr_csf cpsr_csfx cpsr_csx cpsr_csxf cpsr_ctl
	\ cpsr_cx cpsr_cxf cpsr_cxfs cpsr_cxs cpsr_cxsf cpsr_f cpsr_fc
	\ cpsr_fcs cpsr_fcsx cpsr_fcx cpsr_fcxs cpsr_flg cpsr_fs cpsr_fsc
	\ cpsr_fscx cpsr_fsx cpsr_fsxc cpsr_fx cpsr_fxc cpsr_fxcs cpsr_fxs
	\ cpsr_fxsc cpsr_s cpsr_sc cpsr_scf cpsr_scfx cpsr_scx cpsr_scxf
	\ cpsr_sf cpsr_sfc cpsr_sfcx cpsr_sfx cpsr_sfxc cpsr_sx cpsr_sxc
	\ cpsr_sxcf cpsr_sxf cpsr_sxfc cpsr_x cpsr_xc cpsr_xcf cpsr_xcfs
	\ cpsr_xcs cpsr_xcsf cpsr_xf cpsr_xfc cpsr_xfcs cpsr_xfs cpsr_xfsc
	\ cpsr_xs cpsr_xsc cpsr_xscf cpsr_xsf cpsr_xsfc d0 d1 d10 d11 d12 d13
	\ d14 d15 d16 d17 d18 d19 d2 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29
	\ d3 d30 d31 d4 d5 d6 d7 d8 d9 eapsr elr_hyp epsr f0 f1 f2 f3 f4 f5 f6
	\ f7 faultmask fp iapsr iepsr ip ipsr lr lr_abt lr_fiq lr_irq lr_mon
	\ lr_svc lr_und lr_usr msp pc primask psp q0 q1 q10 q11 q12 q13 q14
	\ q15 q2 q3 q4 q5 q6 q7 q8 q9 r10_fiq r10_usr r11_fiq r11_usr r12_fiq
	\ r12_usr r8_fiq r8_usr r9_fiq r9_usr s0 s1 s10 s11 s12 s13 s14 s15
	\ s16 s17 s18 s19 s2 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s3 s30
	\ s31 s4 s5 s6 s7 s8 s9 sb sl sp sp_abt sp_fiq sp_hyp sp_irq sp_mon
	\ sp_svc sp_und sp_usr spsr spsr_abt spsr_all spsr_c spsr_cf spsr_cfs
	\ spsr_cfsx spsr_cfx spsr_cfxs spsr_cs spsr_csf spsr_csfx spsr_csx
	\ spsr_csxf spsr_ctl spsr_cx spsr_cxf spsr_cxfs spsr_cxs spsr_cxsf
	\ spsr_f spsr_fc spsr_fcs spsr_fcsx spsr_fcx spsr_fcxs spsr_fiq
	\ spsr_flg spsr_fs spsr_fsc spsr_fscx spsr_fsx spsr_fsxc spsr_fx
	\ spsr_fxc spsr_fxcs spsr_fxs spsr_fxsc spsr_hyp spsr_irq spsr_mon
	\ spsr_s spsr_sc spsr_scf spsr_scfx spsr_scx spsr_scxf spsr_sf
	\ spsr_sfc spsr_sfcx spsr_sfx spsr_sfxc spsr_svc spsr_sx spsr_sxc
	\ spsr_sxcf spsr_sxf spsr_sxfc spsr_und spsr_x spsr_xc spsr_xcf
	\ spsr_xcfs spsr_xcs spsr_xcsf spsr_xf spsr_xfc spsr_xfcs spsr_xfs
	\ spsr_xfsc spsr_xs spsr_xsc spsr_xscf spsr_xsf spsr_xsfc v1 v2 v3 v4
	\ v5 v6 v7 v8 wcasf wcgr0 wcgr1 wcgr2 wcgr3 wcid wcon wcssf wr0 wr1
	\ wr10 wr11 wr12 wr13 wr14 wr15 wr2 wr3 wr4 wr5 wr6 wr7 wr8 wr9 xpsr
" }}}1

syn keyword fasmarmType p0 p1 p10 p11 p12 p13 p14 p15 p2 p3 p4 p5 p6 p7 p8 p9

" iflags
syn keyword fasmarmType
	\ iflags_a iflags_af iflags_afi iflags_ai iflags_aif iflags_f
	\ iflags_fa iflags_fai iflags_fi iflags_fia iflags_i iflags_ia
	\ iflags_iaf iflags_if iflags_ifa

" Endian
syn keyword fasmarmType be le

" Barrier
syn keyword fasmarmType ish ishst nsh nshst osh oshst st sy syst

syn keyword fasmarmType dspsc

" Condition
syn keyword fasmarmType al cc cs ge gt hi hs lo ls lt mi ne pl vc vs

syn keyword fasmarmType fpexc fpinst fpinst2 fpscr fpsid mvfr0 mvfr1

syn keyword fasmarmType acc0 acc1 acc2 acc3 acc4 acc5 acc6 acc7

" Size operators
syn keyword fasmarmType byte hword word dword

" Unmaintainable stuff {{{1
syn match fasmarmInstruction
	\ '\<\(adc\|adcs\|add\|adds\|adr\|and\|ands\|asr\|asrs\|b\|bic\|bics
	  \\|blx\|bx\|cbnz\|cbz\|cmn\|cmp\|cps\|cpsid\|cpsie\|cpy\|eor\|eors
	  \\|ldm\|ldmfd\|ldmia\|ldr\|ldrb\|ldrh\|ldrsb\|ldrsh\|lsl\|lsls\|lsr
	  \\|lsrs\|mov\|movs\|mul\|muls\|mvn\|mvns\|neg\|negs\|nop\|orr\|orrs
	  \\|pop\|push\|rev\|rev16\|revsh\|ror\|rors\|rsb\|rsbs\|sbc\|sbcs
	  \\|sev\|stm\|stmdb\|stmfd\|stmia\|str\|strb\|strh\|sub\|subs\|svc
	  \\|swi\|sxtb\|sxth\|tst\|und\|uxtb\|uxth\|wfe\|wfi\|yield\)
	  \\(\.[nw]\)\?\>' display
syn match fasmarmInstruction
	\ '\<\(vand\|vbic\|vmvn\|vorn\|vorr\)\(\.i\(16\|32\)\)\?\>' display
syn match fasmarmInstruction
	\ '\<\(vldm\|vldmdb\|vldmea\|vldmfd\|vldmia\|vldr\|vstm\|vstmdb
	  \\|vstmea\|vstmfd\|vstmia\|vstr\)\(\.\(32\|64\)\)\?\>' display
syn match fasmarmInstruction
	\ '\<vmov\(\.\(16\|32\|8\|f\(32\|64\)\|i\(16\|32\|64\|8\)
	  \\|[su]\(16\|8\)\)\)\?\>' display

syn match fasmarmInstruction
	\ '\<\(vaba\|vabal\|vabdl\|vaddl\|vaddw\|vhadd\|vhsub\|vmlal
	  \\|vmlsl\|vmovl\|vpadal\|vpaddl\|vrhadd\|vsubl\|vsubw\)
	  \\.[su]\(16\|32\|8\)\>' display
syn match fasmarmInstruction
	\ '\<\(vabd\|vcge\|vcgt\|vcle\|vclt\|vmax\|vmin\|vpmax\|vpmin\)
	  \\.\(f32\|[su]\(16\|32\|8\)\)\>' display
syn match fasmarmInstruction
	\ '\<\(vabs\|vneg\)\.\(f\(32\|64\)\|s\(16\|32\|8\)\)\>' display
syn match fasmarmInstruction
	\ '\<\(vacge\|vacgt\|vacle\|vaclt\|vrecps\|vrsqrts\)\.f32\>' display
syn match fasmarmInstruction
	\ '\<\(vadd\|vsub\)\.\(f\(32\|64\)\|i\(16\|32\|64\|8\)\)\>' display
syn match fasmarmInstruction
	\ '\<\(vaddhn\|vraddhn\|vrshrn\|vrsubhn\|vshrn\|vsubhn\)
	  \\.i\(16\|32\|64\)\>' display
syn match fasmarmInstruction
	\ '\<\(vceq\|vpadd\)\.\(f32\|i\(16\|32\|8\)\)\>' display
syn match fasmarmInstruction
	\ '\<\(vcls\|vqabs\|vqneg\)\.s\(16\|32\|8\)\>' display
syn match fasmarmInstruction '\<\(vclz\|vmovn\)\.i\(16\|32\|8\)\>' display
syn match fasmarmInstruction
	\ '\<\(vcmp\|vcmpe\|vdiv\|vfma\|vfms\|vfnma\|vfnms\|vnmla
	  \\|vnmls\|vnmul\|vsqrt\)\.f\(32\|64\)\>' display
syn match fasmarmInstruction '\<\(vcnt\|vrev16\)\.8\>' display
syn match fasmarmInstruction
	\ '\<vcvt\.\(f\(16\.f32\|32\.\(f\(16\|64\)\|[su]\(16\|32\)\)
	  \\|64\.\(f32\|[su]\(16\|32\)\)\)\|[su]\(16\|32\)\.f\(32\|64\)\)\>'
	\ display
syn match fasmarmInstruction
	\ '\<\(vcvtb\|vcvtt\)\.f\(16\.f32\|32\.f16\)\>' display
syn match fasmarmInstruction '\<vcvtr\.[su]32\.f\(32\|64\)\>' display
syn match fasmarmInstruction
	\ '\<\(vdup\|vld2\|vld3\|vld4\|vrev64\|vst2\|vst3\|vst4
	  \\|vtrn\|vtst\|vuzp\|vzip\)\.\(16\|32\|8\)\>' display
syn match fasmarmInstruction
	\ '\<\(vext\|vld1\|vsli\|vsri\|vst1\)\.\(16\|32\|64\|8\)\>' display
syn match fasmarmInstruction
	\ '\<\(vmla\|vmls\)\.\(f\(32\|64\)\|[isu]\(16\|32\|8\)\)\>' display
syn match fasmarmInstruction
	\ '\<vmul\.\(f\(32\|64\)\|[isu]\(16\|32\|8\)\|p8\)\>' display
syn match fasmarmInstruction '\<vmull\.\(p8\|[su]\(16\|32\|8\)\)\>' display
syn match fasmarmInstruction
	\ '\<\(vqadd\|vqrshl\|vqshl\|vqsub\|vrshl\|vrshr\|vrsra\|vshr\|vsra\)
	  \\.[su]\(16\|32\|64\|8\)\>' display
syn match fasmarmInstruction
	\ '\<\(vqdmlal\|vqdmlsl\|vqdmulh\|vqdmull\|vqrdmulh\)
	  \\.s\(16\|32\)\>' display
syn match fasmarmInstruction
	\ '\<\(vqmovn\|vqrshrn\|vqshrn\)\.[su]\(16\|32\|64\)\>' display
syn match fasmarmInstruction
	\ '\<\(vqmovun\|vqrshrun\|vqshrun\)\.s\(16\|32\|64\)\>' display
syn match fasmarmInstruction '\<vqshlu\.s\(16\|32\|64\|8\)\>' display
syn match fasmarmInstruction '\<\(vrecpe\|vrsqrte\)\.[fu]32\>' display
syn match fasmarmInstruction '\<vrev32\.\(16\|8\)\>' display
syn match fasmarmInstruction '\<vshl\.[isu]\(16\|32\|64\|8\)\>' display
syn match fasmarmInstruction '\<vshll\.[isu]\(16\|32\|8\)\>' display
syn match fasmarmInstruction '\<\(vtbl\|vtbx\)\.8\>' display
" }}}1

syn match fasmarmLabel
	\ '[^ ;\t]\+\s*:' contained contains=fasmarmSpecial display
syn match fasmarmDummy '^\s*[^ ;\t]\+\s*:' contains=fasmarmLabel display

" Decimal or floating point
syn match fasmarmNumber '\<\d\+\(\.\d\+\)\?\(e[+\-]*\d\+\)\?f\?\>' display

" Binary
syn match fasmarmNumber '\<[01]\+b\>' display

" Octal
syn match fasmarmNumber '\<\o\+o\>' display

" Hexadecimal
syn match fasmarmNumber '\<\d\x*h\>' display
syn match fasmarmNumber '\<0x\x\+\>' display
syn match fasmarmNumber '\$\x\+\>' display

syn match fasmarmOperator '[#&*+,/<=>\-`|~]' display

syn match fasmarmRegister
	\ '\<r\([02-9]\|1[0-5]\?\)\(@\(1\(28\|6\)\|256\|32\|64\)\)\?\>'
	\ display

" Anonymous label
syn match fasmarmSpecial '@@' contained display

" References to anonymous label
syn match fasmarmSpecial '@\(b\|f\|r\)' display

syn match fasmarmSpecial '%t\?' display
syn match fasmarmSpecial '?' display

" XXX: Is there a way to not highlight three or more consecutive $ characters?
syn match fasmarmSpecial '\$' contains=fasmarmNumber display

syn match fasmarmType '[!():\[\\\]\^{}]' display

syn region fasmarmComment start=';' end='$' contains=fasmarmTodo display

syn region fasmarmString start="'" end="'" display oneline
syn region fasmarmString start='"' end='"' display oneline

hi def link fasmarmComment	Comment
hi def link fasmarmDirective	PreProc
hi def link fasmarmInstruction	Keyword
hi def link fasmarmLabel	Label
hi def link fasmarmMacro	Macro
hi def link fasmarmNumber	Number
hi def link fasmarmOperator	Operator
hi def link fasmarmRegister	Type
hi def link fasmarmSpecial	Special
hi def link fasmarmString	String
hi def link fasmarmTodo		Todo
hi def link fasmarmType		Type

let b:current_syntax = "fasmarm"

" vim: fdm=marker:tw=78
