########################################
# OS 別の設定
case ${OSTYPE} in
    darwin*)
        #Mac用の設定
        export CLICOLOR=1
        alias ls='ls -G -F'
        ;;
    linux*)
        #Linux用の設定
        alias ls='ls -F --color=auto'
        ;;
esac

####################
# My Settings
#
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=~/.bin:$PATH

# java
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home
export PATH=$JAVA_HOME/bin:$PATH

# For @Ocaml
if exists 'opam'; then
    # OPAM configuration
    . /Users/nao/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
    eval `opam config env`

    #ocamlspot
    export OCAMLPARAM="_,bin-annot=1"
    export OPAMKEEPBUILDDIR=1
fi

# Golang
export GOPATH=$HOME/repos
export PATH=$PATH:$GOPATH/bin

# for rust
export RUST_SRC_PATH=$HOME/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src
export PATH=$PATH:$HOME/.cargo/bin

# for haskell
export PATH=$PATH:$HOME/.local/bin

export PATH=$PATH:/usr/local/llvm/bin

# latex
PATH=$PATH:/usr/local/texlive/2015/bin/x86_64-darwin/

# anyenv
export PATH="$HOME/.anyenv/bin:$PATH"
eval "`anyenv init -`"

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

## event
function chpwd-ls() {
    ls
}
add-zsh-hook chpwd chpwd-ls

echo "==> Start to load init scripts"
if [[ -d $HOME/.zsh  ]]; then
    for f in $HOME/.zsh/[0-9]*.zsh
    do
        echo "Loading ${f}..."
        source $f
    done
    unset f
fi
