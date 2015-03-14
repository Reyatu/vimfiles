" Nothing To Say Here

" Last Change: 2015 March 14

hi clear

set background=dark

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "ntsh"

" Terminal
hi Directory	term=NONE			ctermfg=6
hi Folded	term=NONE			ctermfg=1
hi Identifier	term=NONE	cterm=NONE
hi MatchParen	term=NONE					ctermbg=4
hi NonText	term=NONE			ctermfg=1
hi Question	term=NONE
hi Search	term=bold	cterm=bold			ctermbg=1
hi SpecialKey					ctermfg=1
hi Statement	term=NONE
hi StatusLine	term=NONE	cterm=NONE	ctermfg=7	ctermbg=4
hi Todo		term=bold	cterm=bold	ctermfg=3	ctermbg=4
hi Type		term=NONE

" GUI
hi Cursor			guifg=Black	guibg=Green
hi CursorLine					guibg=#202020
hi Directory			guifg=Orange
hi Folded			guifg=Red	guibg=Black
hi MatchParen					guibg=Blue
hi ModeMsg			guifg=#f0f0f0
hi NonText	gui=NONE	guifg=DarkRed
hi Normal			guifg=#f0f0f0	guibg=Black
hi Question	gui=NONE
hi Search	gui=bold			guibg=Red
hi SpecialKey			guifg=DarkRed
hi Statement	gui=NONE	guifg=Yellow
hi StatusLine	gui=NONE	guifg=#f0f0f0	guibg=Blue
hi Todo		gui=bold	guifg=Yellow	guibg=Blue
hi Type		gui=NONE	guifg=Green
hi Visual	gui=reverse			guibg=NONE
