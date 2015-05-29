#!/bin/bash

install_git() {
    if type git >/dev/null 2>&1; then
        return
    fi
    echo 'install git...'
    sudo apt-get install git
}

install_git
