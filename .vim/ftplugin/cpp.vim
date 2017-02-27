execute 'setlocal path=.,./src,/usr/include,/usr/local/include,' .
      \ join(filter(split(glob('/usr/include/**/c++/*'), '\n'),
      \             'isdirectory(v:val)'),
      \      ',')

function! s:include_guard(...)
    if a:0 > 0
        let name = a:1
    else
        let file = toupper(expand('%'))
        let name = substitute(file, '[/\.]', '_', 'g')
    endif
    let head = "#ifndef " . name . "\n#define " . name . "\n\n"
    let foot = '#endif // ' . name
    silent! execute '1s/^/\=head'
    silent! execute '$s/$/\=foot'
endfunction

command! -nargs=? IncludeGuard call s:include_guard(<f-args>)
