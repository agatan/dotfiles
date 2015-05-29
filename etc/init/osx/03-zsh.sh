#!/bin/bash

install_zsh() {
    if type zsh >/dev/null 2>&1; then
        return
    fi
    echo 'install zsh...'
    brew install zsh
}

install_zsh

