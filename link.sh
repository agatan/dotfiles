#!/bin/bash

echo 'making symbolic links...'

ln -s ${HOME}/dotfiles/vim ${HOME}/.vim

for dot in '.zshrc' '.gitignore' '.gitconfig' '.editorconfig'
do
    ln -s ${HOME}/dotfiles/${dot} ${HOME}/${dot}
done

neovim_link()
{
  nvimpath=${XDG_CONFIG_HOME:-${HOME}/.config/nvim}
  mkdir -p $nvimpath
  ln -s ${HOME}/dotfiles/init.vim $nvimpath/init.vim
}

neovim_link

ln -s ${HOME}/dotfiles/peco ${HOME}/.config/peco
