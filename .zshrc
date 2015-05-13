# 少し凝った zshrc
# License : MIT
# http://mollifier.mit-license.or/

########################################
# 環境変数
export LANG=ja_JP.UTF-8


# 色を使用出来るようにする
autoload -Uz colors
colors
export LSCOLORS=gxfxcxdxbxegedabagacad
export LS_COLORS='di=36:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*' list-colors \
'di=36' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
# emacs 風キーバインドにする
bindkey -e

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# プロンプト
# 1行表示
# PROMPT="%~ %# "
# 2行表示
local p_cdir="%B%F{red}[%~]%f%b"$'\n'
local p_info="%n@%m"
local p_mark="%B%(?,%F{green},%F{red})%%%f%b"
PROMPT=" $p_cdir$p_info $p_mark "
RPROMPT='[%d]'

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
fpath=(/usr/local/share/zsh-completions $fpath)
autoload -Uz compinit
compinit -u

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
# vcs_info

autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%1(v|%F{green}%1v%f|)"


########################################
# オプション
# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# beep を無効にする
setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# '#' 以降をコメントとして扱う
setopt interactive_comments

# ディレクトリ名だけでcdする
setopt auto_cd

# cd したら自動的にpushdする
setopt auto_pushd
# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# = の後はパス名として補完する
setopt magic_equal_subst

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# ヒストリファイルに保存するとき、すでに重複したコマンドがあったら古い方を削除する
setopt hist_save_nodups

# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# 補完候補が複数あるときに自動的に一覧表示する
setopt auto_menu

# 高機能なワイルドカード展開を使用する
setopt extended_glob

########################################
# キーバインド

# ^R で履歴検索をするときに * でワイルドカードを使用出来るようにする
bindkey '^R' history-incremental-pattern-search-backward

########################################
# エイリアス

alias la='ls -a'
alias ll='ls -l'

alias rm='rm'
alias cp='cp -i'
alias mv='mv -i'

alias mkdir='mkdir -p'

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

# グローバルエイリアス
alias -g L='| less'
alias -g G='| grep'

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
        ;;
esac

# vim:set ft=zsh:
#
PATH=~/.bin:$PATH
export PATH


export PROJECT_HOME=$HOME/workspace

JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_51.jdk/Contents/Home
export JAVA_HOME

PATH=${JAVA_HOME}/bin:$PATH
export PATH



if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi


##
# Your previous /Users/nao/.bash_profile file was backed up as /Users/nao/.bash_profile.macports-saved_2014-05-26_at_13:04:42
##

# MacPorts Installer addition on 2014-05-26_at_13:04:42: adding an appropriate PATH variable for use with MacPorts.
# Finished adapting your PATH environment variable for use with MacPorts.


##
# Your previous /Users/nao/.bash_profile file was backed up as /Users/nao/.bash_profile.macports-saved_2014-05-26_at_13:09:01
##

# MacPorts Installer addition on 2014-05-26_at_13:09:01: adding an appropriate PATH variable for use with MacPorts.

# Finished adapting your PATH environment variable for use with MacPorts.

export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH

# For Haskell (Cabal)
export PATH=/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH
CABAL_HOME=$HOME/.cabal/
export CABAL_HOME
PATH=${CABAL_HOME}bin:$PATH
export PATH



# For @Ocaml
#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/nao/.gvm/bin/gvm-init.sh" ]] && source "/Users/nao/.gvm/bin/gvm-init.sh"

# OPAM configuration
. /Users/nao/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
eval `opam config env`

#ocamlspot
export OCAMLPARAM="_,bin-annot=1"
export OPAMKEEPBUILDDIR=1

# mono
export MONO_GAC_PREFIX="/usr/local"

#nodebrew
export PATH=$HOME/.nodebrew/current/bin:$PATH

# Atom
export ATOM_PATH=/opt/homebrew-cask/Caskroom/atom/latest

# python virtualenv
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

# Docker
export DOCKER_HOST=tcp://192.168.59.104:2376
export DOCKER_CERT_PATH=/Users/nao/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1
# rbenv
eval "$(rbenv init -)"

# bundle
# bundle exec の入力を簡単に
alias bunexec='bundle exec '

# android コマンドラインツールのパス

# Golang
export GOROOT=/usr/local/go
export GOPATH=$GOROOT/packages
export PATH=$PATH:$GOROOT/bin

# haskell
export PATH="$HOME/Library/Haskell/bin:$PATH"

# llvm
export PATH=/usr/local/opt/llvm/bin:$PATH

# Android SDK
export ANDROID_HOME=/usr/local/opt/android-sdk

# Add environment variable COCOS_CONSOLE_ROOT for cocos2d-x
export COCOS_CONSOLE_ROOT=/Users/nao/cocos2d-x-3.3/tools/cocos2d-console/bin
export PATH=$COCOS_CONSOLE_ROOT:$PATH

# Add environment variable COCOS_X_ROOT for cocos2d-x
export COCOS_X_ROOT=/Users/nao/cocos2d-x-3.3
export PATH=$COCOS_X_ROOT:$PATH

# Add environment variable COCOS_TEMPLATES_ROOT for cocos2d-x
export COCOS_TEMPLATES_ROOT=/Users/nao/cocos2d-x-3.3/templates
export PATH=$COCOS_TEMPLATES_ROOT:$PATH

# Add environment variable NDK_ROOT for cocos2d-x
export NDK_ROOT=/usr/local/Cellar/android-ndk/r10d
export PATH=$NDK_ROOT:$PATH

# Add environment variable ANDROID_SDK_ROOT for cocos2d-x
export ANDROID_SDK_ROOT=/usr/local/Cellar/android-sdk/24.0.1
export PATH=$ANDROID_SDK_ROOT:$PATH
export PATH=$ANDROID_SDK_ROOT/tools:$ANDROID_SDK_ROOT/platform-tools:$PATH

export RUST_SRC_PATH=~/rust/rust/src


alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
