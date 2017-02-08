# zplug
if [ ! -r "$HOME/.zplug/init.zsh" ]; then
    printf 'Install zplug? [y/N]: '
    if read -q; then
        echo; curl -sL zplug.sh/installer | zsh
    else
        exit
    fi
fi
source $HOME/.zplug/init.zsh

zplug 'zsh-users/zsh-autosuggestions'
zplug 'zsh-users/zsh-completions'
zplug 'zsh-users/zsh-syntax-highlighting', defer:2
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

if ! zplug check --verbose; then
    printf 'Install? [y/N]: '
    if read -q; then
        echo; zplug install
    fi
fi

zplug load --verbose

echo "==> Start to load init scripts"
if [[ -d $HOME/.zsh  ]]; then
    for f in $HOME/.zsh/[0-9]*.zsh
    do
        echo "Loading ${f}..."
        source $f
    done
    unset f
fi
