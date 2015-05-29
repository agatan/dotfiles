#!/bin/bash

install_brew() {
    if type brew >/dev/null 2>&1; then
        return
    fi
    echo 'install homebrew...'
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
}

install_brew
