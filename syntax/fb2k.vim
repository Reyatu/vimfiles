" Last Change: 2015 January 28

syn keyword fb2kTodo FIXME NOTE TODO XXX contained

syn case ignore

syn match fb2kError '//' display

syn match fb2kMacro '\$\w\+' display

syn match fb2kNumber '\<\d\+\>' display

syn match fb2kOperator '[,<>]' display

syn match fb2kType '[()\[\]]' display

syn region fb2kComment start='^//' end='$' contains=fb2kTodo display

syn region fb2kSpecial start='%' end='%'

syn region fb2kString start="'" end="'"

hi def link fb2kComment		Comment
hi def link fb2kError		Error
hi def link fb2kMacro		Macro
hi def link fb2kNumber		Number
hi def link fb2kOperator	Operator
hi def link fb2kSpecial		Special
hi def link fb2kString		String
hi def link fb2kTodo		Todo
hi def link fb2kType		Type
