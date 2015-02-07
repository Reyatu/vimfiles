" Mostly copied from FASM.PDF and TABLES.INC

" Last Change: 2015 February 07

syn keyword fasmTodo FIXME NOTE TODO XXX contained

" Macros for basic Windows headers
syn keyword fasmMacro
	\ accelerator addr api bitmap c ccall cinvoke comcall cominvk cursor
	\ dialog dialogitem directory enddialog endf endl endp endres ends
	\ fastcall float frame icon interface invoke library locals menu
	\ menuitem menuseparator proc resdata stdcall struct union uses
	\ versioninfo

" Macros for extended Windows headers
syn keyword fasmMacro double elseif endif endw signed until

" Symbolic variables for customizing procedures
syn match fasmSpecial
	\ '\<\(close\|epilogue\|localbase\|parmbase\|prologue\)@proc\>'
	\ display

" Conditional flags for extended Windows headers
syn match fasmSpecial '\<\(CARRY\|OVERFLOW\|PARITY\|SIGN\|ZERO\)?' display

" Ignore case from here
syn case ignore

" Data directives
syn keyword fasmDirective db dd df dp dq dt du dw file rb rd rf rp rq rt rw

" Special operator for data directives
syn keyword fasmDirective dup

" Control directives
syn keyword fasmDirective
	\ label if end else used defined relativeto times repeat break while
	\ org load from store at virtual align display err assert

" Preprocessor directives
syn keyword fasmDirective
	\ include equ restore define fix macro purge postpone struc restruc
	\ rept irp irps irpv match

" Formatter directives
syn keyword fasmDirective
	\ format binary as use16 use32 use64 mz heap segment entry stack pe
	\ console gui native efi efiboot efiruntime dll wdm large on pe64
	\ section code data readable writeable writable executable shareable
	\ discardable notpageable export import resource fixups coff ms ms64
	\ linkremove linkinfo extrn public static elf elf64 interpreter
	\ dynamic note

" Undocumented flag for PE format
" http://board.flatassembler.net/topic.php?p=132492#132492
syn keyword fasmDirective nx

" fasmInstruction {{{1
syn keyword fasmInstruction
	\ aaa aad aam aas adc adcx add addpd addps addsd addss addsubpd
	\ addsubps adox aesdec aesdeclast aesenc aesenclast aesimc
	\ aeskeygenassist and andn andnpd andnps andpd andps arpl bextr
	\ blcfill blci blcic blcmsk blcs blendpd blendps blendvpd blendvps
	\ blsfill blsi blsic blsmsk blsr bound bsf bsr bswap bt btc btr bts
	\ bzhi call cbw cdq cdqe clac clc cld clflush clgi cli clts cmc cmova
	\ cmovae cmovb cmovbe cmovc cmove cmovg cmovge cmovl cmovle cmovna
	\ cmovnae cmovnb cmovnbe cmovnc cmovne cmovng cmovnge cmovnl cmovnle
	\ cmovno cmovnp cmovns cmovnz cmovo cmovp cmovpe cmovpo cmovs cmovz
	\ cmp cmpeqpd cmpeqps cmpeqsd cmpeqss cmplepd cmpleps cmplesd cmpless
	\ cmpltpd cmpltps cmpltsd cmpltss cmpneqpd cmpneqps cmpneqsd cmpneqss
	\ cmpnlepd cmpnleps cmpnlesd cmpnless cmpnltpd cmpnltps cmpnltsd
	\ cmpnltss cmpordpd cmpordps cmpordsd cmpordss cmppd cmpps cmps cmpsb
	\ cmpsd cmpsq cmpss cmpsw cmpunordpd cmpunordps cmpunordsd cmpunordss
	\ cmpxchg cmpxchg16b cmpxchg8b comisd comiss cpuid cqo crc32 cvtdq2pd
	\ cvtdq2ps cvtpd2dq cvtpd2pi cvtpd2ps cvtpi2pd cvtpi2ps cvtps2dq
	\ cvtps2pd cvtps2pi cvtsd2si cvtsd2ss cvtsi2sd cvtsi2ss cvtss2sd
	\ cvtss2si cvttpd2dq cvttpd2pi cvttps2dq cvttps2pi cvttsd2si cvttss2si
	\ cwd cwde daa das dec div divpd divps divsd divss dppd dpps emms
	\ enter extractps extrq f2xm1 fabs fadd faddp fbld fbstp fchs fclex
	\ fcmovb fcmovbe fcmove fcmovnb fcmovnbe fcmovne fcmovnu fcmovu fcom
	\ fcomi fcomip fcomp fcompp fcos fdecstp fdisi fdiv fdivp fdivr fdivrp
	\ femms feni ffree ffreep fiadd ficom ficomp fidiv fidivr fild fimul
	\ fincstp finit fist fistp fisttp fisub fisubr fld fld1 fldcw fldenv
	\ fldenvd fldenvw fldl2e fldl2t fldlg2 fldln2 fldpi fldz fmul fmulp
	\ fnclex fndisi fneni fninit fnop fnsave fnsaved fnsavew fnstcw
	\ fnstenv fnstenvd fnstenvw fnstsw fpatan fprem fprem1 fptan frndint
	\ frstor frstord frstorw frstpm fsave fsaved fsavew fscale fsetpm fsin
	\ fsincos fsqrt fst fstcw fstenv fstenvd fstenvw fstp fstsw fsub fsubp
	\ fsubr fsubrp ftst fucom fucomi fucomip fucomp fucompp fwait fxam
	\ fxch fxrstor fxrstor64 fxsave fxsave64 fxtract fyl2x fyl2xp1 getsec
	\ haddpd haddps hlt hsubpd hsubps icebp idiv imul in inc ins insb insd
	\ insertps insertq insw int int1 int3 into invd invept invlpg invlpga
	\ invpcid invvpid iret iretd iretq iretw ja jae jb jbe jc jcxz je
	\ jecxz jg jge jl jle jmp jna jnae jnb jnbe jnc jne jng jnge jnl jnle
	\ jno jnp jns jnz jo jp jpe jpo jrcxz js jz lahf lar lddqu ldmxcsr lds
	\ lea leave les lfence lfs lgdt lgs lidt lldt llwpcb lmsw loadall286
	\ loadall386 lock lods lodsb lodsd lodsq lodsw loop loopd loope looped
	\ loopeq loopew loopne loopned loopneq loopnew loopnz loopnzd loopnzq
	\ loopnzw loopq loopw loopz loopzd loopzq loopzw lsl lss ltr lwpins
	\ lwpval lzcnt maskmovdqu maskmovq maxpd maxps maxsd maxss mfence
	\ minpd minps minsd minss monitor mov movapd movaps movbe movd movddup
	\ movdq2q movdqa movdqu movhlps movhpd movhps movlhps movlpd movlps
	\ movmskpd movmskps movntdq movntdqa movnti movntpd movntps movntq
	\ movntsd movntss movq movq2dq movs movsb movsd movshdup movsldup
	\ movsq movss movsw movsx movsxd movupd movups movzx mpsadbw mul mulpd
	\ mulps mulsd mulss mulx mwait neg nop not or orpd orps out outs outsb
	\ outsd outsw pabsb pabsd pabsw packssdw packsswb packusdw packuswb
	\ paddb paddd paddq paddsb paddsw paddusb paddusw paddw palignr pand
	\ pandn pause pavgb pavgusb pavgw pblendvb pblendw pclmulhqhdq
	\ pclmulhqhqdq pclmulhqlqdq pclmullqhdq pclmullqhqdq pclmullqlqdq
	\ pclmulqdq pcmpeqb pcmpeqd pcmpeqq pcmpeqw pcmpestri pcmpestrm
	\ pcmpgtb pcmpgtd pcmpgtq pcmpgtw pcmpistri pcmpistrm pdep pext pextrb
	\ pextrd pextrq pextrw pf2id pf2iw pfacc pfadd pfcmpeq pfcmpge pfcmpgt
	\ pfmax pfmin pfmul pfnacc pfpnacc pfrcp pfrcpit1 pfrcpit2 pfrsqit1
	\ pfrsqrt pfsub pfsubr phaddd phaddsw phaddw phminposuw phsubd phsubsw
	\ phsubw pi2fd pi2fw pinsrb pinsrd pinsrq pinsrw pmaddubsw pmaddwd
	\ pmaxsb pmaxsd pmaxsw pmaxub pmaxud pmaxuw pminsb pminsd pminsw
	\ pminub pminud pminuw pmovmskb pmovsxbd pmovsxbq pmovsxbw pmovsxdq
	\ pmovsxwd pmovsxwq pmovzxbd pmovzxbq pmovzxbw pmovzxdq pmovzxwd
	\ pmovzxwq pmuldq pmulhrsw pmulhrw pmulhuw pmulhw pmulld pmullw
	\ pmuludq pop popa popad popaw popcnt popd popf popfd popfq popfw popq
	\ popw por prefetch prefetchnta prefetcht0 prefetcht1 prefetcht2
	\ prefetchw psadbw pshufb pshufd pshufhw pshuflw pshufw psignb psignd
	\ psignw pslld pslldq psllq psllw psrad psraw psrld psrldq psrlq psrlw
	\ psubb psubd psubq psubsb psubsw psubusb psubusw psubw pswapd ptest
	\ punpckhbw punpckhdq punpckhqdq punpckhwd punpcklbw punpckldq
	\ punpcklqdq punpcklwd push pusha pushad pushaw pushd pushf pushfd
	\ pushfq pushfw pushq pushw pxor rcl rcpps rcpss rcr rdfsbase rdgsbase
	\ rdmsr rdmsrq rdpmc rdrand rdseed rdtsc rdtscp rep repe repne repnz
	\ repz ret retd retf retfd retfq retfw retn retnd retnq retnw retq
	\ retw rol ror rorx roundpd roundps roundsd roundss rsm rsqrtps
	\ rsqrtss sahf sal salc sar sarx sbb scas scasb scasd scasq scasw seta
	\ setae setalc setb setbe setc sete setg setge setl setle setna setnae
	\ setnb setnbe setnc setne setng setnge setnl setnle setno setnp setns
	\ setnz seto setp setpe setpo sets setz sfence sgdt shl shld shlx shr
	\ shrd shrx shufpd shufps sidt skinit sldt slwpcb smsw sqrtpd sqrtps
	\ sqrtsd sqrtss stac stc std stgi sti stmxcsr stos stosb stosd stosq
	\ stosw str sub subpd subps subsd subss swapgs syscall sysenter
	\ sysexit sysexitq sysret sysretq t1mskc test tzcnt tzmsk ucomisd
	\ ucomiss ud2 unpckhpd unpckhps unpcklpd unpcklps vaddpd vaddps vaddsd
	\ vaddss vaddsubpd vaddsubps vaesdec vaesdeclast vaesenc vaesenclast
	\ vaesimc vaeskeygenassist vandnpd vandnps vandpd vandps vblendpd
	\ vblendps vblendvpd vblendvps vbroadcastf128 vbroadcasti128
	\ vbroadcastsd vbroadcastss vcmpeq_ospd vcmpeq_osps vcmpeq_ossd
	\ vcmpeq_osss vcmpeq_uqpd vcmpeq_uqps vcmpeq_uqsd vcmpeq_uqss
	\ vcmpeq_uspd vcmpeq_usps vcmpeq_ussd vcmpeq_usss vcmpeqpd vcmpeqps
	\ vcmpeqsd vcmpeqss vcmpfalse_ospd vcmpfalse_osps vcmpfalse_ossd
	\ vcmpfalse_osss vcmpfalsepd vcmpfalseps vcmpfalsesd vcmpfalsess
	\ vcmpge_oqpd vcmpge_oqps vcmpge_oqsd vcmpge_oqss vcmpgepd vcmpgeps
	\ vcmpgesd vcmpgess vcmpgt_oqpd vcmpgt_oqps vcmpgt_oqsd vcmpgt_oqss
	\ vcmpgtpd vcmpgtps vcmpgtsd vcmpgtss vcmple_oqpd vcmple_oqps
	\ vcmple_oqsd vcmple_oqss vcmplepd vcmpleps vcmplesd vcmpless
	\ vcmplt_oqpd vcmplt_oqps vcmplt_oqsd vcmplt_oqss vcmpltpd vcmpltps
	\ vcmpltsd vcmpltss vcmpneq_oqpd vcmpneq_oqps vcmpneq_oqsd
	\ vcmpneq_oqss vcmpneq_ospd vcmpneq_osps vcmpneq_ossd vcmpneq_osss
	\ vcmpneq_uspd vcmpneq_usps vcmpneq_ussd vcmpneq_usss vcmpneqpd
	\ vcmpneqps vcmpneqsd vcmpneqss vcmpnge_uqpd vcmpnge_uqps vcmpnge_uqsd
	\ vcmpnge_uqss vcmpngepd vcmpngeps vcmpngesd vcmpngess vcmpngt_uqpd
	\ vcmpngt_uqps vcmpngt_uqsd vcmpngt_uqss vcmpngtpd vcmpngtps vcmpngtsd
	\ vcmpngtss vcmpnle_uqpd vcmpnle_uqps vcmpnle_uqsd vcmpnle_uqss
	\ vcmpnlepd vcmpnleps vcmpnlesd vcmpnless vcmpnlt_uqpd vcmpnlt_uqps
	\ vcmpnlt_uqsd vcmpnlt_uqss vcmpnltpd vcmpnltps vcmpnltsd vcmpnltss
	\ vcmpord_spd vcmpord_sps vcmpord_ssd vcmpord_sss vcmpordpd vcmpordps
	\ vcmpordsd vcmpordss vcmppd vcmpps vcmpsd vcmpss vcmptrue_uspd
	\ vcmptrue_usps vcmptrue_ussd vcmptrue_usss vcmptruepd vcmptrueps
	\ vcmptruesd vcmptruess vcmpunord_spd vcmpunord_sps vcmpunord_ssd
	\ vcmpunord_sss vcmpunordpd vcmpunordps vcmpunordsd vcmpunordss
	\ vcomisd vcomiss vcvtdq2pd vcvtdq2ps vcvtpd2dq vcvtpd2ps vcvtph2ps
	\ vcvtps2dq vcvtps2pd vcvtps2ph vcvtsd2si vcvtsd2ss vcvtsi2sd
	\ vcvtsi2ss vcvtss2sd vcvtss2si vcvttpd2dq vcvttps2dq vcvttsd2si
	\ vcvttss2si vdivpd vdivps vdivsd vdivss vdppd vdpps verr verw
	\ vextractf128 vextracti128 vextractps vfmadd132pd vfmadd132ps
	\ vfmadd132sd vfmadd132ss vfmadd213pd vfmadd213ps vfmadd213sd
	\ vfmadd213ss vfmadd231pd vfmadd231ps vfmadd231sd vfmadd231ss vfmaddpd
	\ vfmaddps vfmaddsd vfmaddss vfmaddsub132pd vfmaddsub132ps
	\ vfmaddsub213pd vfmaddsub213ps vfmaddsub231pd vfmaddsub231ps
	\ vfmaddsubpd vfmaddsubps vfmsub132pd vfmsub132ps vfmsub132sd
	\ vfmsub132ss vfmsub213pd vfmsub213ps vfmsub213sd vfmsub213ss
	\ vfmsub231pd vfmsub231ps vfmsub231sd vfmsub231ss vfmsubadd132pd
	\ vfmsubadd132ps vfmsubadd213pd vfmsubadd213ps vfmsubadd231pd
	\ vfmsubadd231ps vfmsubaddpd vfmsubaddps vfmsubpd vfmsubps vfmsubsd
	\ vfmsubss vfnmadd132pd vfnmadd132ps vfnmadd132sd vfnmadd132ss
	\ vfnmadd213pd vfnmadd213ps vfnmadd213sd vfnmadd213ss vfnmadd231pd
	\ vfnmadd231ps vfnmadd231sd vfnmadd231ss vfnmaddpd vfnmaddps vfnmaddsd
	\ vfnmaddss vfnmsub132pd vfnmsub132ps vfnmsub132sd vfnmsub132ss
	\ vfnmsub213pd vfnmsub213ps vfnmsub213sd vfnmsub213ss vfnmsub231pd
	\ vfnmsub231ps vfnmsub231sd vfnmsub231ss vfnmsubpd vfnmsubps vfnmsubsd
	\ vfnmsubss vfrczpd vfrczps vfrczsd vfrczss vgatherdpd vgatherdps
	\ vgatherqpd vgatherqps vhaddpd vhaddps vhsubpd vhsubps vinsertf128
	\ vinserti128 vinsertps vlddqu vldmxcsr vmaskmovdqu vmaskmovpd
	\ vmaskmovps vmaxpd vmaxps vmaxsd vmaxss vmcall vmclear vminpd vminps
	\ vminsd vminss vmlaunch vmload vmmcall vmovapd vmovaps vmovd vmovddup
	\ vmovdqa vmovdqu vmovhlps vmovhpd vmovhps vmovlhps vmovlpd vmovlps
	\ vmovmskpd vmovmskps vmovntdq vmovntdqa vmovntpd vmovntps vmovq
	\ vmovsd vmovshdup vmovsldup vmovss vmovupd vmovups vmpsadbw vmptrld
	\ vmptrst vmread vmresume vmrun vmsave vmulpd vmulps vmulsd vmulss
	\ vmwrite vmxoff vmxon vorpd vorps vpabsb vpabsd vpabsw vpackssdw
	\ vpacksswb vpackusdw vpackuswb vpaddb vpaddd vpaddq vpaddsb vpaddsw
	\ vpaddusb vpaddusw vpaddw vpalignr vpand vpandn vpavgb vpavgw
	\ vpblendd vpblendvb vpblendw vpbroadcastb vpbroadcastd vpbroadcastq
	\ vpbroadcastw vpclmulhqhdq vpclmulhqlqdq vpclmullqhdq vpclmullqlqdq
	\ vpclmulqdq vpcmov vpcmpeqb vpcmpeqd vpcmpeqq vpcmpeqw vpcmpestri
	\ vpcmpestrm vpcmpgtb vpcmpgtd vpcmpgtq vpcmpgtw vpcmpistri vpcmpistrm
	\ vpcomb vpcomd vpcomeqb vpcomeqd vpcomeqq vpcomequb vpcomequd
	\ vpcomequq vpcomequw vpcomeqw vpcomfalseb vpcomfalsed vpcomfalseq
	\ vpcomfalseub vpcomfalseud vpcomfalseuq vpcomfalseuw vpcomfalsew
	\ vpcomgeb vpcomged vpcomgeq vpcomgeub vpcomgeud vpcomgeuq vpcomgeuw
	\ vpcomgew vpcomgtb vpcomgtd vpcomgtq vpcomgtub vpcomgtud vpcomgtuq
	\ vpcomgtuw vpcomgtw vpcomleb vpcomled vpcomleq vpcomleub vpcomleud
	\ vpcomleuq vpcomleuw vpcomlew vpcomltb vpcomltd vpcomltq vpcomltub
	\ vpcomltud vpcomltuq vpcomltuw vpcomltw vpcomneqb vpcomneqd vpcomneqq
	\ vpcomnequb vpcomnequd vpcomnequq vpcomnequw vpcomneqw vpcomq
	\ vpcomtrueb vpcomtrued vpcomtrueq vpcomtrueub vpcomtrueud vpcomtrueuq
	\ vpcomtrueuw vpcomtruew vpcomub vpcomud vpcomuq vpcomuw vpcomw
	\ vperm2f128 vperm2i128 vpermd vpermil2pd vpermil2ps vpermilmo2pd
	\ vpermilmo2ps vpermilmz2pd vpermilmz2ps vpermilpd vpermilps
	\ vpermiltd2pd vpermiltd2ps vpermpd vpermps vpermq vpextrb vpextrd
	\ vpextrq vpextrw vpgatherdd vpgatherdq vpgatherqd vpgatherqq vphaddbd
	\ vphaddbq vphaddbw vphaddd vphadddq vphaddsw vphaddubd vphaddubq
	\ vphaddubw vphaddudq vphadduwd vphadduwq vphaddw vphaddwd vphaddwq
	\ vphminposuw vphsubbw vphsubd vphsubdq vphsubsw vphsubw vphsubwd
	\ vpinsrb vpinsrd vpinsrq vpinsrw vpmacsdd vpmacsdqh vpmacsdql
	\ vpmacssdd vpmacssdqh vpmacssdql vpmacsswd vpmacssww vpmacswd
	\ vpmacsww vpmadcsswd vpmadcswd vpmaddubsw vpmaddwd vpmaskmovd
	\ vpmaskmovq vpmaxsb vpmaxsd vpmaxsw vpmaxub vpmaxud vpmaxuw vpminsb
	\ vpminsd vpminsw vpminub vpminud vpminuw vpmovmskb vpmovsxbd
	\ vpmovsxbq vpmovsxbw vpmovsxdq vpmovsxwd vpmovsxwq vpmovzxbd
	\ vpmovzxbq vpmovzxbw vpmovzxdq vpmovzxwd vpmovzxwq vpmuldq vpmulhrsw
	\ vpmulhuw vpmulhw vpmulld vpmullw vpmuludq vpor vpperm vprotb vprotd
	\ vprotq vprotw vpsadbw vpshab vpshad vpshaq vpshaw vpshlb vpshld
	\ vpshlq vpshlw vpshufb vpshufd vpshufhw vpshuflw vpsignb vpsignd
	\ vpsignw vpslld vpslldq vpsllq vpsllvd vpsllvq vpsllw vpsrad vpsravd
	\ vpsraw vpsrld vpsrldq vpsrlq vpsrlvd vpsrlvq vpsrlw vpsubb vpsubd
	\ vpsubq vpsubsb vpsubsw vpsubusb vpsubusw vpsubw vptest vpunpckhbw
	\ vpunpckhdq vpunpckhqdq vpunpckhwd vpunpcklbw vpunpckldq vpunpcklqdq
	\ vpunpcklwd vpxor vrcpps vrcpss vroundpd vroundps vroundsd vroundss
	\ vrsqrtps vrsqrtss vshufpd vshufps vsqrtpd vsqrtps vsqrtsd vsqrtss
	\ vstmxcsr vsubpd vsubps vsubsd vsubss vtestpd vtestps vucomisd
	\ vucomiss vunpckhpd vunpckhps vunpcklpd vunpcklps vxorpd vxorps
	\ vzeroall vzeroupper wait wbinvd wrfsbase wrgsbase wrmsr wrmsrq
	\ xabort xacquire xadd xbegin xchg xend xgetbv xlat xlatb xor xorpd
	\ xorps xrelease xrstor xrstor64 xsave xsave64 xsaveopt xsaveopt64
	\ xsetbv xtest
" }}}1

" Case insensitive macros
syn keyword fasmMacro common forward local reverse

syn keyword fasmOperator eq eqtype mod ptr

" Special operators
syn keyword fasmOperator plt rva

" fasmRegister {{{1
" General-purpose registers
syn keyword fasmRegister
	\ ah al ax bh bl bp bpl bx ch cl cx dh di dil dl dx eax ebp ebx ecx
	\ edi edx esi esp r10 r10b r10d r10l r10w r11 r11b r11d r11l r11w r12
	\ r12b r12d r12l r12w r13 r13b r13d r13l r13w r14 r14b r14d r14l r14w
	\ r15 r15b r15d r15l r15w r8 r8b r8d r8l r8w r9 r9b r9d r9l r9w rax
	\ rbp rbx rcx rdi rdx rsi rsp si sil sp spl

" Segment registers
syn keyword fasmRegister cs ds es fs gs ss

" x87 FPU registers
syn keyword fasmRegister st st0 st1 st2 st3 st4 st5 st6 st7

" MMX registers
syn keyword fasmRegister mm0 mm1 mm2 mm3 mm4 mm5 mm6 mm7

" XMM registers
syn keyword fasmRegister
	\ xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm9 xmm10 xmm11 xmm12
	\ xmm13 xmm14 xmm15

" YMM registers
syn keyword fasmRegister
	\ ymm0 ymm1 ymm2 ymm3 ymm4 ymm5 ymm6 ymm7 ymm8 ymm9 ymm10 ymm11 ymm12
	\ ymm13 ymm14 ymm15

" Control registers
syn keyword fasmRegister
	\ cr0 cr1 cr2 cr3 cr4 cr5 cr6 cr7 cr8 cr9 cr10 cr11 cr12 cr13 cr14
	\ cr15

" Debug registers
syn keyword fasmRegister
	\ dr0 dr1 dr2 dr3 dr4 dr5 dr6 dr7 dr8 dr9 dr10 dr11 dr12 dr13 dr14
	\ dr15

" XXX: Task registers?
syn keyword fasmRegister tr0 tr1 tr2 tr3 tr4 tr5 tr6 tr7
" }}}1

" Size operators
syn keyword fasmType
	\ byte dword qword word
	\ dqword fword pword qqword tbyte tword xword yword

" Jump and call types
syn keyword fasmType far near short

syn match fasmLabel '^\s*\zs[^ ;\t]\+\s*:' contains=fasmSpecial display

" Decimal or floating point
syn match fasmNumber '\<\d\+\(\.\d\+\)\?\(e[+\-]*\d\+\)\?f\?\>' display

" Binary
syn match fasmNumber '\<[01]\+b\>' display

" Octal
syn match fasmNumber '\<\o\+o\>' display

" Hexadecimal
syn match fasmNumber '\<\d\x*h\>' display
syn match fasmNumber '\<0x\x\+\>' display
syn match fasmNumber '\>\@!\$\x\+\>' display

syn match fasmOperator '[#&*+,/<=>\-`|~]' display

" Anonymous label
syn match fasmSpecial '@@' contained display

" Reference to anonymous label
syn match fasmSpecial '\>\@!@[bfr]\>' display

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
