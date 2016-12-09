execute 'setlocal path=.,/usr/include,/usr/local/include,' .
      \ join(filter(split(glob('/usr/include/**/c++/*'), '\n'),
      \             'isdirectory(v:val)'),
      \      ',')
