typeset -gx -U path
path=( \
    /usr/local/bin(N-/) \
    ~/bin(N-/) \
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

# golang
export GOPATH=$HOME/repos
export PATH="$GOPATH/bin:$PATH"

# rust
if [[ -d "$HOME/.cargo/bin" ]]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi
