#!/bin/bash

# if you should build vim, this function returns 0
should_build_vim() {
    if ! type vim >/dev/null 2>&1; then
        return
    fi
    if ! vim --version | grep +lua >/dev/null 2>&1; then
        return
    fi
    # if installed vim is too old
    if ! vim --version | grep "Vi IMproved 7" >/dev/null 2>&1; then
        return
    fi
    return 1
}

fetch_vim() {
    cd /usr/local/src
    sudo git clone https://github.com/vim/vim
    sudo ./configure --with-features=huge \
                     --enable-multibyte \
                     --enable-perlinterp \
                     --enable-pythoninterp \
                     --enable-rubyinterp \
                     --enable-luainterp=dynamic \
                     --enable-fail-if-missing
    make
    sudo make install
}

if should_build_vim; then
    fetch_vim
fi
