#!/bin/bash

echo 'making symbolic links...'

ln -s ${HOME}/dotfiles/vim ${HOME}/.vim

for dot in '.zshrc'
do
    ln -s ${HOME}/dotfiles/${dot} ${HOME}/${dot}
done

mkdir -p ~/.config/nvim
ln -s ${HOME}/dotfiles/init.vim ${HOME}/.config/nvim/init.vim
