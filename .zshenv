typeset -gx -U path
path=( \
    /usr/local/bin(N-/) \
    ~/bin(N-/) \
    ~/.local/bin(N-/) \
    ~/.zplug/bin(N-/) \
    "$path[@]"
    )

autoload -Uz run-help
autoload -Uz colors && colors
autoload -Uz compinit && compinit

export LANGUAGE="en_US.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

export EDITOR=vim

export PAGER=less
export LESS='-R -f -i'

# DO NOT OVERRIDE PATHes
setopt no_global_rcs

#### Language

# golang
export GOPATH=$HOME/repos
export PATH="$GOPATH/bin:$PATH"

# rust
if [[ -d "$HOME/.cargo/bin" ]]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# History
export HISTFILE=~/.zsh_history
export HISTSIZE=1000000
export SAVEHIST=1000000
export LISTMAX=50
## Do not add histories in root
if [[ $UID == 0 ]]; then
    unset HISTFILE
    export SAVEHIST=0
fi
