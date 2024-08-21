" scrunch syntax highlighting
"
" to enable copy this file into your .config/nvim/syntax and put this in init.lua
" vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
"   pattern = "*.scr",
"   command = "set filetype=scrunch",
" })

if exists("b:current_syntax")
  finish
endif

" defun is treated as a keyword just because of how common it is, even though
" it's not a built-in keyword, but a macro in the standard library
syntax keyword scrunchKeywords if cond eq? list car cdr cons lambda define macro println write writeln display import export defun

syntax region scrunchComment start=";" end="$"

syntax region scrunchNumber start=/\d/ end=/\ze[ )]/

syntax region scrunchString start=/"/ skip=/\\"/ end=/"/

highlight default link scrunchKeywords Keyword
highlight default link scrunchComment Comment
highlight default link scrunchNumber Number
highlight default link scrunchString String

let b:current_syntax = "scrunch"
