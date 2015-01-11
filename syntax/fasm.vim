" Mostly copied from FASM.PDF and TABLES.INC

" Last Change: 2015 January 11

syn keyword fasmTodo FIXME NOTE TODO XXX contained

" Macros for basic Windows headers
syn keyword fasmMacro accelerator addr api bitmap c ccall cinvoke comcall
syn keyword fasmMacro cominvk cursor dialog dialogitem directory enddialog
syn keyword fasmMacro endf endl endp endres ends fastcall float frame icon
syn keyword fasmMacro interface invoke library locals menu menuitem
syn keyword fasmMacro menuseparator proc resdata stdcall struct union uses
syn keyword fasmMacro versioninfo

" Macros for extended Windows headers
syn keyword fasmMacro double elseif endif endw signed until

" Symbolic variables for customizing procedures
syn match fasmSpecial '\<\(close\|epilogue\|localbase\|parmbase\|prologue\)
		      \@proc\>' display

" Conditional flags for extended Windows headers
syn match fasmSpecial '\<\(CARRY\|OVERFLOW\|PARITY\|SIGN\|ZERO\)?' display

" Ignore case from here
syn case ignore

" Data directives
syn keyword fasmDirective db dd df dp dq dt du dw file rb rd rf rp rq rt rw

" Special operator for data directives
syn keyword fasmDirective dup

" Control directives
syn keyword fasmDirective label if end else used defined relativeto times
syn keyword fasmDirective repeat break while org load from store at virtual
syn keyword fasmDirective align display err assert

" Preprocessor directives
syn keyword fasmDirective include equ restore define fix macro purge postpone
syn keyword fasmDirective struc restruc rept irp irps irpv match

" Formatter directives
syn keyword fasmDirective format binary as use16 use32 use64 mz heap segment
syn keyword fasmDirective entry stack pe console gui native efi efiboot
syn keyword fasmDirective efiruntime dll wdm large on pe64 section code data
syn keyword fasmDirective readable writeable writable executable shareable
syn keyword fasmDirective discardable notpageable export import resource
syn keyword fasmDirective fixups coff ms ms64 linkremove linkinfo extrn public
syn keyword fasmDirective static elf elf64 interpreter dynamic note

" Undocumented flag for PE format
" http://board.flatassembler.net/topic.php?p=132492#132492
syn keyword fasmDirective nx

" fasmInstruction {{{1
syn keyword fasmInstruction aaa aad aam aas adc adcx add addpd addps addsd
syn keyword fasmInstruction addss addsubpd addsubps adox aesdec aesdeclast
syn keyword fasmInstruction aesenc aesenclast aesimc aeskeygenassist and andn
syn keyword fasmInstruction andnpd andnps andpd andps arpl bextr blcfill blci
syn keyword fasmInstruction blcic blcmsk blcs blendpd blendps blendvpd
syn keyword fasmInstruction blendvps blsfill blsi blsic blsmsk blsr bound bsf
syn keyword fasmInstruction bsr bswap bt btc btr bts bzhi call cbw cdq cdqe
syn keyword fasmInstruction clac clc cld clflush clgi cli clts cmc cmova
syn keyword fasmInstruction cmovae cmovb cmovbe cmovc cmove cmovg cmovge cmovl
syn keyword fasmInstruction cmovle cmovna cmovnae cmovnb cmovnbe cmovnc cmovne
syn keyword fasmInstruction cmovng cmovnge cmovnl cmovnle cmovno cmovnp cmovns
syn keyword fasmInstruction cmovnz cmovo cmovp cmovpe cmovpo cmovs cmovz cmp
syn keyword fasmInstruction cmpeqpd cmpeqps cmpeqsd cmpeqss cmplepd cmpleps
syn keyword fasmInstruction cmplesd cmpless cmpltpd cmpltps cmpltsd cmpltss
syn keyword fasmInstruction cmpneqpd cmpneqps cmpneqsd cmpneqss cmpnlepd
syn keyword fasmInstruction cmpnleps cmpnlesd cmpnless cmpnltpd cmpnltps
syn keyword fasmInstruction cmpnltsd cmpnltss cmpordpd cmpordps cmpordsd
syn keyword fasmInstruction cmpordss cmppd cmpps cmps cmpsb cmpsd cmpsq cmpss
syn keyword fasmInstruction cmpsw cmpunordpd cmpunordps cmpunordsd cmpunordss
syn keyword fasmInstruction cmpxchg cmpxchg16b cmpxchg8b comisd comiss cpuid
syn keyword fasmInstruction cqo crc32 cvtdq2pd cvtdq2ps cvtpd2dq cvtpd2pi
syn keyword fasmInstruction cvtpd2ps cvtpi2pd cvtpi2ps cvtps2dq cvtps2pd
syn keyword fasmInstruction cvtps2pi cvtsd2si cvtsd2ss cvtsi2sd cvtsi2ss
syn keyword fasmInstruction cvtss2sd cvtss2si cvttpd2dq cvttpd2pi cvttps2dq
syn keyword fasmInstruction cvttps2pi cvttsd2si cvttss2si cwd cwde daa das dec
syn keyword fasmInstruction div divpd divps divsd divss dppd dpps emms enter
syn keyword fasmInstruction extractps extrq f2xm1 fabs fadd faddp fbld fbstp
syn keyword fasmInstruction fchs fclex fcmovb fcmovbe fcmove fcmovnb fcmovnbe
syn keyword fasmInstruction fcmovne fcmovnu fcmovu fcom fcomi fcomip fcomp
syn keyword fasmInstruction fcompp fcos fdecstp fdisi fdiv fdivp fdivr fdivrp
syn keyword fasmInstruction femms feni ffree ffreep fiadd ficom ficomp fidiv
syn keyword fasmInstruction fidivr fild fimul fincstp finit fist fistp fisttp
syn keyword fasmInstruction fisub fisubr fld fld1 fldcw fldenv fldenvd fldenvw
syn keyword fasmInstruction fldl2e fldl2t fldlg2 fldln2 fldpi fldz fmul fmulp
syn keyword fasmInstruction fnclex fndisi fneni fninit fnop fnsave fnsaved
syn keyword fasmInstruction fnsavew fnstcw fnstenv fnstenvd fnstenvw fnstsw
syn keyword fasmInstruction fpatan fprem fprem1 fptan frndint frstor frstord
syn keyword fasmInstruction frstorw frstpm fsave fsaved fsavew fscale fsetpm
syn keyword fasmInstruction fsin fsincos fsqrt fst fstcw fstenv fstenvd
syn keyword fasmInstruction fstenvw fstp fstsw fsub fsubp fsubr fsubrp ftst
syn keyword fasmInstruction fucom fucomi fucomip fucomp fucompp fwait fxam
syn keyword fasmInstruction fxch fxrstor fxrstor64 fxsave fxsave64 fxtract
syn keyword fasmInstruction fyl2x fyl2xp1 getsec haddpd haddps hlt hsubpd
syn keyword fasmInstruction hsubps icebp idiv imul in inc ins insb insd
syn keyword fasmInstruction insertps insertq insw int int1 int3 into invd
syn keyword fasmInstruction invept invlpg invlpga invpcid invvpid iret iretd
syn keyword fasmInstruction iretq iretw ja jae jb jbe jc jcxz je jecxz jg jge
syn keyword fasmInstruction jl jle jmp jna jnae jnb jnbe jnc jne jng jnge jnl
syn keyword fasmInstruction jnle jno jnp jns jnz jo jp jpe jpo jrcxz js jz
syn keyword fasmInstruction lahf lar lddqu ldmxcsr lds lea leave les lfence
syn keyword fasmInstruction lfs lgdt lgs lidt lldt llwpcb lmsw loadall286
syn keyword fasmInstruction loadall386 lock lods lodsb lodsd lodsq lodsw loop
syn keyword fasmInstruction loopd loope looped loopeq loopew loopne loopned
syn keyword fasmInstruction loopneq loopnew loopnz loopnzd loopnzq loopnzw
syn keyword fasmInstruction loopq loopw loopz loopzd loopzq loopzw lsl lss ltr
syn keyword fasmInstruction lwpins lwpval lzcnt maskmovdqu maskmovq maxpd
syn keyword fasmInstruction maxps maxsd maxss mfence minpd minps minsd minss
syn keyword fasmInstruction monitor mov movapd movaps movbe movd movddup
syn keyword fasmInstruction movdq2q movdqa movdqu movhlps movhpd movhps
syn keyword fasmInstruction movlhps movlpd movlps movmskpd movmskps movntdq
syn keyword fasmInstruction movntdqa movnti movntpd movntps movntq movntsd
syn keyword fasmInstruction movntss movq movq2dq movs movsb movsd movshdup
syn keyword fasmInstruction movsldup movsq movss movsw movsx movsxd movupd
syn keyword fasmInstruction movups movzx mpsadbw mul mulpd mulps mulsd mulss
syn keyword fasmInstruction mulx mwait neg nop not or orpd orps out outs outsb
syn keyword fasmInstruction outsd outsw pabsb pabsd pabsw packssdw packsswb
syn keyword fasmInstruction packusdw packuswb paddb paddd paddq paddsb paddsw
syn keyword fasmInstruction paddusb paddusw paddw palignr pand pandn pause
syn keyword fasmInstruction pavgb pavgusb pavgw pblendvb pblendw pclmulhqhdq
syn keyword fasmInstruction pclmulhqhqdq pclmulhqlqdq pclmullqhdq pclmullqhqdq
syn keyword fasmInstruction pclmullqlqdq pclmulqdq pcmpeqb pcmpeqd pcmpeqq
syn keyword fasmInstruction pcmpeqw pcmpestri pcmpestrm pcmpgtb pcmpgtd
syn keyword fasmInstruction pcmpgtq pcmpgtw pcmpistri pcmpistrm pdep pext
syn keyword fasmInstruction pextrb pextrd pextrq pextrw pf2id pf2iw pfacc
syn keyword fasmInstruction pfadd pfcmpeq pfcmpge pfcmpgt pfmax pfmin pfmul
syn keyword fasmInstruction pfnacc pfpnacc pfrcp pfrcpit1 pfrcpit2 pfrsqit1
syn keyword fasmInstruction pfrsqrt pfsub pfsubr phaddd phaddsw phaddw
syn keyword fasmInstruction phminposuw phsubd phsubsw phsubw pi2fd pi2fw
syn keyword fasmInstruction pinsrb pinsrd pinsrq pinsrw pmaddubsw pmaddwd
syn keyword fasmInstruction pmaxsb pmaxsd pmaxsw pmaxub pmaxud pmaxuw pminsb
syn keyword fasmInstruction pminsd pminsw pminub pminud pminuw pmovmskb
syn keyword fasmInstruction pmovsxbd pmovsxbq pmovsxbw pmovsxdq pmovsxwd
syn keyword fasmInstruction pmovsxwq pmovzxbd pmovzxbq pmovzxbw pmovzxdq
syn keyword fasmInstruction pmovzxwd pmovzxwq pmuldq pmulhrsw pmulhrw pmulhuw
syn keyword fasmInstruction pmulhw pmulld pmullw pmuludq pop popa popad popaw
syn keyword fasmInstruction popcnt popd popf popfd popfq popfw popq popw por
syn keyword fasmInstruction prefetch prefetchnta prefetcht0 prefetcht1
syn keyword fasmInstruction prefetcht2 prefetchw psadbw pshufb pshufd pshufhw
syn keyword fasmInstruction pshuflw pshufw psignb psignd psignw pslld pslldq
syn keyword fasmInstruction psllq psllw psrad psraw psrld psrldq psrlq psrlw
syn keyword fasmInstruction psubb psubd psubq psubsb psubsw psubusb psubusw
syn keyword fasmInstruction psubw pswapd ptest punpckhbw punpckhdq punpckhqdq
syn keyword fasmInstruction punpckhwd punpcklbw punpckldq punpcklqdq punpcklwd
syn keyword fasmInstruction push pusha pushad pushaw pushd pushf pushfd pushfq
syn keyword fasmInstruction pushfw pushq pushw pxor rcl rcpps rcpss rcr
syn keyword fasmInstruction rdfsbase rdgsbase rdmsr rdmsrq rdpmc rdrand rdseed
syn keyword fasmInstruction rdtsc rdtscp rep repe repne repnz repz ret retd
syn keyword fasmInstruction retf retfd retfq retfw retn retnd retnq retnw retq
syn keyword fasmInstruction retw rol ror rorx roundpd roundps roundsd roundss
syn keyword fasmInstruction rsm rsqrtps rsqrtss sahf sal salc sar sarx sbb
syn keyword fasmInstruction scas scasb scasd scasq scasw seta setae setalc
syn keyword fasmInstruction setb setbe setc sete setg setge setl setle setna
syn keyword fasmInstruction setnae setnb setnbe setnc setne setng setnge setnl
syn keyword fasmInstruction setnle setno setnp setns setnz seto setp setpe
syn keyword fasmInstruction setpo sets setz sfence sgdt shl shld shlx shr shrd
syn keyword fasmInstruction shrx shufpd shufps sidt skinit sldt slwpcb smsw
syn keyword fasmInstruction sqrtpd sqrtps sqrtsd sqrtss stac stc std stgi sti
syn keyword fasmInstruction stmxcsr stos stosb stosd stosq stosw str sub subpd
syn keyword fasmInstruction subps subsd subss swapgs syscall sysenter sysexit
syn keyword fasmInstruction sysexitq sysret sysretq t1mskc test tzcnt tzmsk
syn keyword fasmInstruction ucomisd ucomiss ud2 unpckhpd unpckhps unpcklpd
syn keyword fasmInstruction unpcklps vaddpd vaddps vaddsd vaddss vaddsubpd
syn keyword fasmInstruction vaddsubps vaesdec vaesdeclast vaesenc vaesenclast
syn keyword fasmInstruction vaesimc vaeskeygenassist vandnpd vandnps vandpd
syn keyword fasmInstruction vandps vblendpd vblendps vblendvpd vblendvps
syn keyword fasmInstruction vbroadcastf128 vbroadcasti128 vbroadcastsd
syn keyword fasmInstruction vbroadcastss vcmpeq_ospd vcmpeq_osps vcmpeq_ossd
syn keyword fasmInstruction vcmpeq_osss vcmpeq_uqpd vcmpeq_uqps vcmpeq_uqsd
syn keyword fasmInstruction vcmpeq_uqss vcmpeq_uspd vcmpeq_usps vcmpeq_ussd
syn keyword fasmInstruction vcmpeq_usss vcmpeqpd vcmpeqps vcmpeqsd vcmpeqss
syn keyword fasmInstruction vcmpfalse_ospd vcmpfalse_osps vcmpfalse_ossd
syn keyword fasmInstruction vcmpfalse_osss vcmpfalsepd vcmpfalseps vcmpfalsesd
syn keyword fasmInstruction vcmpfalsess vcmpge_oqpd vcmpge_oqps vcmpge_oqsd
syn keyword fasmInstruction vcmpge_oqss vcmpgepd vcmpgeps vcmpgesd vcmpgess
syn keyword fasmInstruction vcmpgt_oqpd vcmpgt_oqps vcmpgt_oqsd vcmpgt_oqss
syn keyword fasmInstruction vcmpgtpd vcmpgtps vcmpgtsd vcmpgtss vcmple_oqpd
syn keyword fasmInstruction vcmple_oqps vcmple_oqsd vcmple_oqss vcmplepd
syn keyword fasmInstruction vcmpleps vcmplesd vcmpless vcmplt_oqpd vcmplt_oqps
syn keyword fasmInstruction vcmplt_oqsd vcmplt_oqss vcmpltpd vcmpltps vcmpltsd
syn keyword fasmInstruction vcmpltss vcmpneq_oqpd vcmpneq_oqps vcmpneq_oqsd
syn keyword fasmInstruction vcmpneq_oqss vcmpneq_ospd vcmpneq_osps
syn keyword fasmInstruction vcmpneq_ossd vcmpneq_osss vcmpneq_uspd
syn keyword fasmInstruction vcmpneq_usps vcmpneq_ussd vcmpneq_usss vcmpneqpd
syn keyword fasmInstruction vcmpneqps vcmpneqsd vcmpneqss vcmpnge_uqpd
syn keyword fasmInstruction vcmpnge_uqps vcmpnge_uqsd vcmpnge_uqss vcmpngepd
syn keyword fasmInstruction vcmpngeps vcmpngesd vcmpngess vcmpngt_uqpd
syn keyword fasmInstruction vcmpngt_uqps vcmpngt_uqsd vcmpngt_uqss vcmpngtpd
syn keyword fasmInstruction vcmpngtps vcmpngtsd vcmpngtss vcmpnle_uqpd
syn keyword fasmInstruction vcmpnle_uqps vcmpnle_uqsd vcmpnle_uqss vcmpnlepd
syn keyword fasmInstruction vcmpnleps vcmpnlesd vcmpnless vcmpnlt_uqpd
syn keyword fasmInstruction vcmpnlt_uqps vcmpnlt_uqsd vcmpnlt_uqss vcmpnltpd
syn keyword fasmInstruction vcmpnltps vcmpnltsd vcmpnltss vcmpord_spd
syn keyword fasmInstruction vcmpord_sps vcmpord_ssd vcmpord_sss vcmpordpd
syn keyword fasmInstruction vcmpordps vcmpordsd vcmpordss vcmppd vcmpps vcmpsd
syn keyword fasmInstruction vcmpss vcmptrue_uspd vcmptrue_usps vcmptrue_ussd
syn keyword fasmInstruction vcmptrue_usss vcmptruepd vcmptrueps vcmptruesd
syn keyword fasmInstruction vcmptruess vcmpunord_spd vcmpunord_sps
syn keyword fasmInstruction vcmpunord_ssd vcmpunord_sss vcmpunordpd
syn keyword fasmInstruction vcmpunordps vcmpunordsd vcmpunordss vcomisd
syn keyword fasmInstruction vcomiss vcvtdq2pd vcvtdq2ps vcvtpd2dq vcvtpd2ps
syn keyword fasmInstruction vcvtph2ps vcvtps2dq vcvtps2pd vcvtps2ph vcvtsd2si
syn keyword fasmInstruction vcvtsd2ss vcvtsi2sd vcvtsi2ss vcvtss2sd vcvtss2si
syn keyword fasmInstruction vcvttpd2dq vcvttps2dq vcvttsd2si vcvttss2si vdivpd
syn keyword fasmInstruction vdivps vdivsd vdivss vdppd vdpps verr verw
syn keyword fasmInstruction vextractf128 vextracti128 vextractps vfmadd132pd
syn keyword fasmInstruction vfmadd132ps vfmadd132sd vfmadd132ss vfmadd213pd
syn keyword fasmInstruction vfmadd213ps vfmadd213sd vfmadd213ss vfmadd231pd
syn keyword fasmInstruction vfmadd231ps vfmadd231sd vfmadd231ss vfmaddpd
syn keyword fasmInstruction vfmaddps vfmaddsd vfmaddss vfmaddsub132pd
syn keyword fasmInstruction vfmaddsub132ps vfmaddsub213pd vfmaddsub213ps
syn keyword fasmInstruction vfmaddsub231pd vfmaddsub231ps vfmaddsubpd
syn keyword fasmInstruction vfmaddsubps vfmsub132pd vfmsub132ps vfmsub132sd
syn keyword fasmInstruction vfmsub132ss vfmsub213pd vfmsub213ps vfmsub213sd
syn keyword fasmInstruction vfmsub213ss vfmsub231pd vfmsub231ps vfmsub231sd
syn keyword fasmInstruction vfmsub231ss vfmsubadd132pd vfmsubadd132ps
syn keyword fasmInstruction vfmsubadd213pd vfmsubadd213ps vfmsubadd231pd
syn keyword fasmInstruction vfmsubadd231ps vfmsubaddpd vfmsubaddps vfmsubpd
syn keyword fasmInstruction vfmsubps vfmsubsd vfmsubss vfnmadd132pd
syn keyword fasmInstruction vfnmadd132ps vfnmadd132sd vfnmadd132ss
syn keyword fasmInstruction vfnmadd213pd vfnmadd213ps vfnmadd213sd
syn keyword fasmInstruction vfnmadd213ss vfnmadd231pd vfnmadd231ps
syn keyword fasmInstruction vfnmadd231sd vfnmadd231ss vfnmaddpd vfnmaddps
syn keyword fasmInstruction vfnmaddsd vfnmaddss vfnmsub132pd vfnmsub132ps
syn keyword fasmInstruction vfnmsub132sd vfnmsub132ss vfnmsub213pd
syn keyword fasmInstruction vfnmsub213ps vfnmsub213sd vfnmsub213ss
syn keyword fasmInstruction vfnmsub231pd vfnmsub231ps vfnmsub231sd
syn keyword fasmInstruction vfnmsub231ss vfnmsubpd vfnmsubps vfnmsubsd
syn keyword fasmInstruction vfnmsubss vfrczpd vfrczps vfrczsd vfrczss
syn keyword fasmInstruction vgatherdpd vgatherdps vgatherqpd vgatherqps
syn keyword fasmInstruction vhaddpd vhaddps vhsubpd vhsubps vinsertf128
syn keyword fasmInstruction vinserti128 vinsertps vlddqu vldmxcsr vmaskmovdqu
syn keyword fasmInstruction vmaskmovpd vmaskmovps vmaxpd vmaxps vmaxsd vmaxss
syn keyword fasmInstruction vmcall vmclear vminpd vminps vminsd vminss
syn keyword fasmInstruction vmlaunch vmload vmmcall vmovapd vmovaps vmovd
syn keyword fasmInstruction vmovddup vmovdqa vmovdqu vmovhlps vmovhpd vmovhps
syn keyword fasmInstruction vmovlhps vmovlpd vmovlps vmovmskpd vmovmskps
syn keyword fasmInstruction vmovntdq vmovntdqa vmovntpd vmovntps vmovq vmovsd
syn keyword fasmInstruction vmovshdup vmovsldup vmovss vmovupd vmovups
syn keyword fasmInstruction vmpsadbw vmptrld vmptrst vmread vmresume vmrun
syn keyword fasmInstruction vmsave vmulpd vmulps vmulsd vmulss vmwrite vmxoff
syn keyword fasmInstruction vmxon vorpd vorps vpabsb vpabsd vpabsw vpackssdw
syn keyword fasmInstruction vpacksswb vpackusdw vpackuswb vpaddb vpaddd vpaddq
syn keyword fasmInstruction vpaddsb vpaddsw vpaddusb vpaddusw vpaddw vpalignr
syn keyword fasmInstruction vpand vpandn vpavgb vpavgw vpblendd vpblendvb
syn keyword fasmInstruction vpblendw vpbroadcastb vpbroadcastd vpbroadcastq
syn keyword fasmInstruction vpbroadcastw vpclmulhqhdq vpclmulhqlqdq
syn keyword fasmInstruction vpclmullqhdq vpclmullqlqdq vpclmulqdq vpcmov
syn keyword fasmInstruction vpcmpeqb vpcmpeqd vpcmpeqq vpcmpeqw vpcmpestri
syn keyword fasmInstruction vpcmpestrm vpcmpgtb vpcmpgtd vpcmpgtq vpcmpgtw
syn keyword fasmInstruction vpcmpistri vpcmpistrm vpcomb vpcomd vpcomeqb
syn keyword fasmInstruction vpcomeqd vpcomeqq vpcomequb vpcomequd vpcomequq
syn keyword fasmInstruction vpcomequw vpcomeqw vpcomfalseb vpcomfalsed
syn keyword fasmInstruction vpcomfalseq vpcomfalseub vpcomfalseud vpcomfalseuq
syn keyword fasmInstruction vpcomfalseuw vpcomfalsew vpcomgeb vpcomged
syn keyword fasmInstruction vpcomgeq vpcomgeub vpcomgeud vpcomgeuq vpcomgeuw
syn keyword fasmInstruction vpcomgew vpcomgtb vpcomgtd vpcomgtq vpcomgtub
syn keyword fasmInstruction vpcomgtud vpcomgtuq vpcomgtuw vpcomgtw vpcomleb
syn keyword fasmInstruction vpcomled vpcomleq vpcomleub vpcomleud vpcomleuq
syn keyword fasmInstruction vpcomleuw vpcomlew vpcomltb vpcomltd vpcomltq
syn keyword fasmInstruction vpcomltub vpcomltud vpcomltuq vpcomltuw vpcomltw
syn keyword fasmInstruction vpcomneqb vpcomneqd vpcomneqq vpcomnequb
syn keyword fasmInstruction vpcomnequd vpcomnequq vpcomnequw vpcomneqw vpcomq
syn keyword fasmInstruction vpcomtrueb vpcomtrued vpcomtrueq vpcomtrueub
syn keyword fasmInstruction vpcomtrueud vpcomtrueuq vpcomtrueuw vpcomtruew
syn keyword fasmInstruction vpcomub vpcomud vpcomuq vpcomuw vpcomw vperm2f128
syn keyword fasmInstruction vperm2i128 vpermd vpermil2pd vpermil2ps
syn keyword fasmInstruction vpermilmo2pd vpermilmo2ps vpermilmz2pd
syn keyword fasmInstruction vpermilmz2ps vpermilpd vpermilps vpermiltd2pd
syn keyword fasmInstruction vpermiltd2ps vpermpd vpermps vpermq vpextrb
syn keyword fasmInstruction vpextrd vpextrq vpextrw vpgatherdd vpgatherdq
syn keyword fasmInstruction vpgatherqd vpgatherqq vphaddbd vphaddbq vphaddbw
syn keyword fasmInstruction vphaddd vphadddq vphaddsw vphaddubd vphaddubq
syn keyword fasmInstruction vphaddubw vphaddudq vphadduwd vphadduwq vphaddw
syn keyword fasmInstruction vphaddwd vphaddwq vphminposuw vphsubbw vphsubd
syn keyword fasmInstruction vphsubdq vphsubsw vphsubw vphsubwd vpinsrb vpinsrd
syn keyword fasmInstruction vpinsrq vpinsrw vpmacsdd vpmacsdqh vpmacsdql
syn keyword fasmInstruction vpmacssdd vpmacssdqh vpmacssdql vpmacsswd
syn keyword fasmInstruction vpmacssww vpmacswd vpmacsww vpmadcsswd vpmadcswd
syn keyword fasmInstruction vpmaddubsw vpmaddwd vpmaskmovd vpmaskmovq vpmaxsb
syn keyword fasmInstruction vpmaxsd vpmaxsw vpmaxub vpmaxud vpmaxuw vpminsb
syn keyword fasmInstruction vpminsd vpminsw vpminub vpminud vpminuw vpmovmskb
syn keyword fasmInstruction vpmovsxbd vpmovsxbq vpmovsxbw vpmovsxdq vpmovsxwd
syn keyword fasmInstruction vpmovsxwq vpmovzxbd vpmovzxbq vpmovzxbw vpmovzxdq
syn keyword fasmInstruction vpmovzxwd vpmovzxwq vpmuldq vpmulhrsw vpmulhuw
syn keyword fasmInstruction vpmulhw vpmulld vpmullw vpmuludq vpor vpperm
syn keyword fasmInstruction vprotb vprotd vprotq vprotw vpsadbw vpshab vpshad
syn keyword fasmInstruction vpshaq vpshaw vpshlb vpshld vpshlq vpshlw vpshufb
syn keyword fasmInstruction vpshufd vpshufhw vpshuflw vpsignb vpsignd vpsignw
syn keyword fasmInstruction vpslld vpslldq vpsllq vpsllvd vpsllvq vpsllw
syn keyword fasmInstruction vpsrad vpsravd vpsraw vpsrld vpsrldq vpsrlq
syn keyword fasmInstruction vpsrlvd vpsrlvq vpsrlw vpsubb vpsubd vpsubq
syn keyword fasmInstruction vpsubsb vpsubsw vpsubusb vpsubusw vpsubw vptest
syn keyword fasmInstruction vpunpckhbw vpunpckhdq vpunpckhqdq vpunpckhwd
syn keyword fasmInstruction vpunpcklbw vpunpckldq vpunpcklqdq vpunpcklwd vpxor
syn keyword fasmInstruction vrcpps vrcpss vroundpd vroundps vroundsd vroundss
syn keyword fasmInstruction vrsqrtps vrsqrtss vshufpd vshufps vsqrtpd vsqrtps
syn keyword fasmInstruction vsqrtsd vsqrtss vstmxcsr vsubpd vsubps vsubsd
syn keyword fasmInstruction vsubss vtestpd vtestps vucomisd vucomiss vunpckhpd
syn keyword fasmInstruction vunpckhps vunpcklpd vunpcklps vxorpd vxorps
syn keyword fasmInstruction vzeroall vzeroupper wait wbinvd wrfsbase wrgsbase
syn keyword fasmInstruction wrmsr wrmsrq xabort xacquire xadd xbegin xchg xend
syn keyword fasmInstruction xgetbv xlat xlatb xor xorpd xorps xrelease xrstor
syn keyword fasmInstruction xrstor64 xsave xsave64 xsaveopt xsaveopt64 xsetbv
syn keyword fasmInstruction xtest
" }}}1

" Case insensitive macros
syn keyword fasmMacro common forward local reverse

syn keyword fasmOperator eq eqtype mod ptr

" Special operators
syn keyword fasmOperator plt rva

" fasmRegister {{{1
" General-purpose registers
syn keyword fasmRegister ah al ax bh bl bp bpl bx ch cl cx dh di dil dl dx eax
syn keyword fasmRegister ebp ebx ecx edi edx esi esp r10 r10b r10d r10l r10w
syn keyword fasmRegister r11 r11b r11d r11l r11w r12 r12b r12d r12l r12w r13
syn keyword fasmRegister r13b r13d r13l r13w r14 r14b r14d r14l r14w r15 r15b
syn keyword fasmRegister r15d r15l r15w r8 r8b r8d r8l r8w r9 r9b r9d r9l r9w
syn keyword fasmRegister rax rbp rbx rcx rdi rdx rsi rsp si sil sp spl

" Segment registers
syn keyword fasmRegister cs ds es fs gs ss

" x87 FPU registers
syn keyword fasmRegister st st0 st1 st2 st3 st4 st5 st6 st7

" MMX registers
syn keyword fasmRegister mm0 mm1 mm2 mm3 mm4 mm5 mm6 mm7

" XMM registers
syn keyword fasmRegister xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm9
syn keyword fasmRegister xmm10 xmm11 xmm12 xmm13 xmm14 xmm15

" YMM registers
syn keyword fasmRegister ymm0 ymm1 ymm2 ymm3 ymm4 ymm5 ymm6 ymm7 ymm8 ymm9
syn keyword fasmRegister ymm10 ymm11 ymm12 ymm13 ymm14 ymm15

" Control registers
syn keyword fasmRegister cr0 cr1 cr2 cr3 cr4 cr5 cr6 cr7 cr8 cr9 cr10 cr11
syn keyword fasmRegister cr12 cr13 cr14 cr15

" Debug registers
syn keyword fasmRegister dr0 dr1 dr2 dr3 dr4 dr5 dr6 dr7 dr8 dr9 dr10 dr11
syn keyword fasmRegister dr12 dr13 dr14 dr15

" XXX: Task registers?
syn keyword fasmRegister tr0 tr1 tr2 tr3 tr4 tr5 tr6 tr7
" }}}1

" Size operators
syn keyword fasmType byte dword qword word
syn keyword fasmType dqword fword pword qqword tbyte tword xword yword

" Jump and call types
syn keyword fasmType far near short

syn match fasmLabel '[^ ;\t]\+\s*:' contained contains=fasmSpecial display
syn match fasmDummy '^\s*[^ ;\t]\+\s*:' contains=fasmLabel display

" Decimal or floating point
syn match fasmNumber '\<\d\+\(\.\d\+\)\?\(e[+\-]*\d\+\)\?f\?\>' display

" Binary
syn match fasmNumber '\<[01]\+b\>' display

" Octal
syn match fasmNumber '\<\o\+o\>' display

" Hexadecimal
syn match fasmNumber '\<\d\x*h\>' display
syn match fasmNumber '\<0x\x\+\>' display
syn match fasmNumber '\$\x\+\>' display

syn match fasmOperator '[#&*+,/<=>\-`|~]' display

" Anonymous label
syn match fasmSpecial '@@' contained display

" References to anonymous label
syn match fasmSpecial '@\(b\|f\|r\)' display

syn match fasmSpecial '%t\?' display
syn match fasmSpecial '?' display

" XXX: Is there a way to not highlight three or more consecutive $ characters?
syn match fasmSpecial '\$' contains=fasmNumber display

syn match fasmType '[():\[\\\]{}]' display

syn region fasmComment start=';' end='$' contains=fasmTodo display

syn region fasmString start="'" end="'" display oneline
syn region fasmString start='"' end='"' display oneline

hi def link fasmConstant	Constant
hi def link fasmComment		Comment
hi def link fasmDirective	PreProc
hi def link fasmInstruction	Statement
hi def link fasmLabel		Label
hi def link fasmMacro		Macro
hi def link fasmNumber		Number
hi def link fasmOperator	Operator
hi def link fasmRegister	Type
hi def link fasmSpecial		Special
hi def link fasmString		String
hi def link fasmTodo		Todo
hi def link fasmType		Type

let b:current_syntax = 'fasm'

" vim:fdm=marker:tw=78
