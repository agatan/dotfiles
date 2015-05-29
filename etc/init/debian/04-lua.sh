#!/bin/bash

if ! type lua >/dev/null 2>&1; then
    sudo apt-get install lua5.2
fi
