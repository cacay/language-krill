" Vim syntax file
" Language:     SILL
" Maintainer:   Josh Acay <coskuacay@gmail.com>
" Last Change:  Novermber 15, 2015
"
" Version:      1.0
" Url:          TODO
"
" Original Author: Josh Acay <coskuacay@gmail.com>


if exists("b:current_syntax")
  finish
endif


" Identifiers
syn match sillConstructor "\<[A-Z][a-zA-Z0-9_]*\>"
" syn match sillIdent       "\<[a-z][a-zA-Z0-9_]*\>"
syn match sillChannel     "`[a-z][a-zA-Z0-9_]*"

" Reserved symbols--cannot be overloaded.
syn match sillDelimiter  "(\|)\|\[\|\]\|,\|;\|_\|{\|}"



" Structural keywords
syn keyword sillModule module
syn keyword sillTypedef data type
syn keyword sillStructure where
syn keyword sillStatement do let in

" Process keywords
syn keyword sillProcess close wait send recv case of

" Operators
syn keyword sillType 1
syn keyword sillOperator and or & + * -o . -> <-
syn match sillOperator ":\|&\|\*\|+\|-o\|\.\|->\|<-"


" Comments
syn keyword sillCommentTodo TODO FIXME XXX TBD contained
syn match sillLineComment "---*\(.*\)\?$" contains=sillCommentTodo,@Spell
syn region sillBlockComment start="{-"  end="-}" contains=sillBlockComment,sillCommentTodo,@Spell


" Mapping
hi link sillModule            sillStructure
hi link sillStructure         Structure
hi link sillStatement         Statement
hi link sillTypedef           Typedef

hi link sillConstructor       Type
hi link sillType              Type
" hi link sillIdent             Identifier
hi link sillChannel           Identifier
hi link sillProcess           Keyword

hi link sillOperator          Operator

hi link sillBlockComment      sillComment
hi link sillLineComment       sillComment
hi link sillComment           Comment
" hi link sillFunction          Function

let b:current_syntax = "sill"
