#!/bin/bash

install_hg() {
    if type hg >/dev/null 2>&1; then
        return
    fi
    echo 'install hg...'
    brew install hg
}

install_hg
