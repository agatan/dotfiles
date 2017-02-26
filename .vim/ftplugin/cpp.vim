execute 'setlocal path=.,./src,/usr/include,/usr/local/include,' .
      \ join(filter(split(glob('/usr/include/**/c++/*'), '\n'),
      \             'isdirectory(v:val)'),
      \      ',')

function! s:include_guard(name)
    let head = "#ifndef " . a:name . "\n#define " . a:name . "\n\n"
    let foot = '#endif // ' . a:name
    silent! execute '1s/^/\=head'
    silent! execute '$s/$/\=foot'
endfunction

command! -nargs=1 IncludeGuard call s:include_guard(<f-args>)
