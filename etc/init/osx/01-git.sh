#!/bin/bash

install_git() {
    if type git >/dev/null 2>&1; then
        return
    fi
    echo 'install git...'
    brew install git
}

install_git
