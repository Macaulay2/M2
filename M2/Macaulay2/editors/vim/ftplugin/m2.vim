" Authors:     David Cook II <dcook@ms.uky.edu>
"              Manoj Kummini <mkummini@cmi.ac.in>
" License:     Public Domain

" Per-buffer settings
setlocal cpt+=k
setlocal dict+=~/.vim/dict/m2.vim.dict

" Tab: complete from dictionary when cursor is after a word character,
" otherwise insert a literal tab.
if !exists('*M2_Tab_Or_Complete')
    function! M2_Tab_Or_Complete()
        if col('.')>1 && strpart(getline('.'), col('.')-2, 3) =~ '^\w'
            return "\<C-N>"
        else
            return "\<Tab>"
        endif
    endfunction
endif
inoremap <buffer> <Tab> <C-R>=M2_Tab_Or_Complete()<CR>

function! M2Warning(msg)
    echohl WarningMsg
    echo a:msg
    echohl Normal
endfunction

if !executable('M2')
    call M2Warning('vim-M2-plugin: M2 not found on PATH')
    finish
endif

let g:m2_terminal_buf = get(g:, 'm2_terminal_buf', -1)

command! -nargs=0 M2Start      :call M2Start()
command! -nargs=0 M2Restart    :call M2Restart()
command! -nargs=0 M2Exit       :call M2Exit()
command! -nargs=1 M2SendString :call M2SendString(<args>)
command! -nargs=0 M2SendBuffer :call M2SendBuffer()
command! -nargs=0 -range M2Send :call M2Send(<line1>,<line2>)

nnoremap <buffer> <F12> :M2Start<CR>
noremap  <buffer> <F11> :M2Send<CR>j0
inoremap <buffer> <F11> <ESC>:M2Send<CR>o

function! M2IsRunning()
    if g:m2_terminal_buf == -1 || !bufexists(g:m2_terminal_buf)
        return 0
    endif
    let job = term_getjob(g:m2_terminal_buf)
    return job isnot 0 && job_status(job) ==# 'run'
endfunction

" Open M2 in a terminal split, or switch to it if already running.
function! M2Start()
    if M2IsRunning()
        execute 'buffer ' . g:m2_terminal_buf
        return
    endif
    let g:m2_terminal_buf = term_start('M2', {'term_name': 'Macaulay2', 'term_finish': 'open'})
endfunction

function! M2Restart()
    call M2SendString('restart')
endfunction

function! M2Exit()
    call M2SendString('exit')
endfunction

function! M2SendString(str)
    if !M2IsRunning()
        call M2Warning('vim-M2-plugin: M2 is not running. Use :M2Start or <F12>.')
        return
    endif
    call term_sendkeys(g:m2_terminal_buf, a:str . "\<CR>")
endfunction

function! M2SendBuffer()
    call M2Send('1', '$')
endfunction

" Send a string, list, or line range to M2.
function! M2Send(...)
    if a:0 == 0
        let lines = [getline('.')]
    elseif a:0 == 1
        let ta1 = type(a:1)
        if ta1 == 1
            let lines = split(a:1, "\n")
        elseif ta1 == 3
            let lines = a:1
        else
            call M2Warning('vim-M2-plugin: argument must be a string or a list.')
            return
        endif
    elseif a:0 == 2
        if type(a:1) <= 1 && type(a:2) <= 1
            let lines = getline(a:1, a:2)
            let mode = visualmode(1)
            if mode != '' && line("'<") == a:1
                if mode == 'v'
                    let start = col("'<") - 1
                    let end = col("'>") - 1
                    let lines[-1] = lines[-1][: end]
                    let lines[0] = lines[0][start :]
                elseif mode == "\<c-v>"
                    let start = min([col("'<"), col("'>")]) - 1
                    call map(lines, 'v:val[start :]')
                endif
            endif
        else
            call M2Warning('vim-M2-plugin: arguments must be a pair of strings/integers.')
            return
        endif
    else
        call M2Warning('vim-M2-plugin: invalid number of arguments.')
        return
    endif
    for lin in lines
        call M2SendString(lin)
    endfor
endfunction
