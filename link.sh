#!/bin/bash

echo 'making symbolic links...'

for dot in .??*
do
    if [ ${dot} = ".git" -o ${dot} = ".DS_Store" ]; then
        continue
    fi
    ln -s ${HOME}/dotfiles/${dot} ${HOME}/${dot}
done
