" Copied and pasted from FlMML's documentation

" Last Change: 2015 January 11

syn case ignore

syn match flmmlMacro '\$\w\+' display

syn match flmmlNumber '\d\+' display

syn match flmmlOperator '[#&()\*+,\-./:;<=>\[\]]' display

syn match flmmlPreProc '#\(OCTAVE\|VELOCITY\) REVERSE' display

syn match flmmlSpecial '@\(E[1-2]\|MH\|NS\?\|U[0-3]\|[DFLPQVWXiors]
		       \\|\'[AEIOU]\?\'\|\(1[0-4]\|\d\)\|pl\)' display
syn match flmmlSpecial 'NS\|[LQTVX{}]' display

syn match flmmlType '[ABCDEFGOR]' display

syn region flmmlComment start='/\*' end='\*/'

syn region flmmlPreProc start='#OP[MN]@\d\+ {' end='}'
syn region flmmlPreProc start='#\(ARTIST\|CODING\|COMMENT\|FMGAIN
			      \\|PRAGMA\|TITLE\|USING POLY\|WAV
			      \\(1\(0\|3\)\|9\)\)' end='$' display

syn region flmmlString start='"' end='"'

hi def link flmmlComment	Comment
hi def link flmmlMacro		Macro
hi def link flmmlNumber		Number
hi def link flmmlOperator	Operator
hi def link flmmlPreProc	PreProc
hi def link flmmlSpecial	Special
hi def link flmmlString		String
hi def link flmmlType		Type

let b:current_syntax = "flmml"
