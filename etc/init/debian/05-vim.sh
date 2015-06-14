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
    sudo apt-get build-dep vim
    sudo apt-get install lua5.1 liblua5.1-0-dev
    sudo apt-get install luajit libluajit-5.1-2
    sudo apt-get install python-dev
    cd /usr/local/src
    sudo git clone https://github.com/vim/vim
    cd /usr/local/src/vim
    sudo ./configure --with-features=huge \
		     --disable-darwin \
                     --disable-selinux \
                     --enable-multibyte \
                     --enable-pythoninterp \
                     --with-python-config-dir=/usr/lib/python2.7/config-x86_64-linux-gnu \
                     --enable-luainterp=dynamic \
                     --with-luajit \
                     --with-lua-prefix=/usr/local \
                     --enable-fail-if-missing
    sudo make
    sudo make install
}

if should_build_vim; then
    fetch_vim
fi
