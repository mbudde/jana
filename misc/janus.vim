" Vim syntax file
" Language:	Janus
" Maintainer:	Michael Budde <mbudde@gmail.com>
" Last Change:	March 2013


if exists("b:current_syntax")
  finish
endif

syn keyword janusKeyword swap local delocal push pop nil
syn keyword janusKeyword call uncall
syn keyword janusStatement procedure nextgroup=janusProcedure skipwhite
syn keyword janusConditional if then else fi
syn keyword janusRepeat from do loop until
syn keyword janusOperator empty top

syn keyword janusType int stack

syn match   janusProcedure "\w\+" contained

syn match   janusNumber	"\<\%([1-9]\d*\|0\)\>"

syn keyword janusCommentTodo   TODO FIXME XXX TBD contained
syn match   janusLineComment   "\/\/.*" contains=janusCommentTodo
syn region  janusComment       start="/\*" end="\*/" contains=janusCommentTodo


hi def link janusNumber         Number
hi def link janusProcedure      Function
hi def link janusStatement      Statement
hi def link janusConditional    Conditional
hi def link janusRepeat         Repeat
hi def link janusOperator       Operator
hi def link janusKeyword        Keyword
hi def link janusType           Type
hi def link janusComment        Comment
hi def link janusLineComment    Comment
hi def link janusCommentTodo    Todo


let b:current_syntax = "janus"
