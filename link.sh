#!/bin/bash

echo 'making symbolic links...'

ln -s ${HOME}/dotfiles/vim ${HOME}/.vim

for dot in '.zshrc' '.gitignore' '.gitconfig' '.editorconfig' '.tmux.conf' '.tmux'
do
    ln -s ${HOME}/dotfiles/${dot} ${HOME}/${dot}
done
