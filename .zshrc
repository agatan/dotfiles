# Return if zsh is in vim
if [[ -n $VIMRUNTIME ]]; then
    return 0
fi

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

zplug 'zsh-users/zsh-history-substring-search'
zplug 'zsh-users/zsh-autosuggestions'
zplug 'zsh-users/zsh-completions'
zplug 'b4b4r07/git-open', as:command
zplug 'BurntSushi/ripgrep', \
    from:gh-r, \
    as:command, \
    rename-to:"rg"
zplug 'stedolan/jq', \
    from:gh-r, \
    as:command
zplug 'junegunn/fzf-bin', \
    as:command, \
    from:gh-r, \
    rename-to:"fzf", \
    frozen:1
zplug 'junegunn/fzf', \
    as:command, \
    use:"bin/fzf-tmux"
zplug 'b4b4r07/enhancd', use:init.sh
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
        echo "  Loading ${f}..."
        source $f
    done
    unset f
fi

