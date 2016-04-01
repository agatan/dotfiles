execute 'setlocal path=.,/usr/include,/usr/local/include' .
      \ join(filter(split(glob('/usr/include/**/c++/*'), '\n'),
      \             'isdirectory(v:val)'),
      \      ',')

augroup mycpp
  autocmd!
  autocmd BufWritePre *.{c,cpp,h,hpp} SortInclude
augroup END
