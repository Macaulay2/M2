setfiletype m2
runtime! $VIMRUNTIME/syntax/m2.vim
set cpt+=k
set dict+=~/.vim/dict/m2.vim.dict

" For David Cook's macros.
noremap <F12> :M2Start<CR>
noremap <F11> :M2Send<CR>j0
inoremap <F11> <ESC>:M2Send<CR>o

" From Tip 566 Vim Wikia
"Use TAB to complete when typing words, else inserts TABs as usual.
"Uses dictionary and source files to find matching words to complete.
"See help completion for source,
"Note: usual completion is on <C-n> but more trouble to press all the time.
"Never type the same word twice and maybe learn a new spellings!
"Use the Linux dictionary when spelling is in doubt.
"Window users can copy the file to their machine.
function! Tab_Or_Complete()
  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
    return "\<C-N>"
  else
    return "\<Tab>"
  endif
endfunction
inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
