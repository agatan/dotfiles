export LANG=ja_JP.UTF-8

autoload -Uz colors
colors

# emacs 風キーバインドにする
bindkey -e

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# vim から :sh している場合
if [[ -n "$VIMRUNTIME" ]]; then
    PROMPT="%{${fg[white]}${bg[blue]}%}(vim)%{${reset_color}%} $PROMPT"
fi


# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default
# ここで指定した文字は単語区切りとみなされる
# / も区切りと扱うので、^W でディレクトリ１つ分を削除できる
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

########################################
# 補完
# 補完機能を有効にする
autoload -Uz compinit
compinit

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..

# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                   /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'




########################################
# オプション
# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# beep を無効にする
setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# vim から入った sh でない場合は Ctrl+Dでzshを終了しない
if [[ -z "$VIMRUNTIME" ]]; then
    setopt ignore_eof
fi

# '#' 以降をコメントとして扱う
setopt interactive_comments

# cd したら自動的にpushdする
setopt auto_pushd
# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# 高機能なワイルドカード展開を使用する
setopt extended_glob

########################################
# キーバインド

# ^R で履歴検索をするときに * でワイルドカードを使用出来るようにする
bindkey '^R' history-incremental-pattern-search-backward

########################################
# エイリアス

alias cp='cp -i'
alias mv='mv -i'

alias mkdir='mkdir -p'

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

# C で標準出力をクリップボードにコピーする
# mollifier delta blog : http://mollifier.hatenablog.com/entry/20100317/p1
if which pbcopy >/dev/null 2>&1 ; then
    # Mac
    alias -g C='| pbcopy'
elif which xsel >/dev/null 2>&1 ; then
    # Linux
    alias -g C='| xsel --input --clipboard'
elif which putclip >/dev/null 2>&1 ; then
    # Cygwin
    alias -g C='| putclip'
fi



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
