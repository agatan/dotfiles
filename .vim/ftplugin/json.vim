function! s:json_fmt() abort
    if !executable('jq')
        throw "command not found: 'jq'"
    endif
    %!jq .
endfunction

command! JSONFmt call s:json_fmt()
