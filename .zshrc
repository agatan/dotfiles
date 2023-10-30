# Avoid registering duplicated path entries.
typeset -U path cdpath fpath manpath

executable() {
  type "$1" >/dev/null 2>&1
}

if executable nvim; then
  export EDITOR=nvim
elif executable vim; then
  export EDITOR=vim
elif executable vi; then
  export EDITOR=vi
elif executable emacs; then
  export EDTIOR=emacs
fi

bindkey -e

export PATH=$HOME/bin:$PATH
export GOPATH=$HOME/repos
export PATH=$GOPATH/bin:$PATH

ARCH=$(uname -m)
if [[ $ARCH == arm64 ]]; then
  [ -x /opt/homebrew/bin/brew ] && eval $(/opt/homebrew/bin/brew shellenv)
elif [[ $ARCH == x86_64 ]]; then
  [ -x /usr/local/bin/brew ] && eval $(/usr/local/bin/brew shellenv)
fi

if [ ! -r $HOME/.asdf/asdf.sh ]; then
  echo "asdf is not installed." >&2
  echo "\tgit clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.1" >&2
fi
if [ -r $HOME/.asdf/asdf.sh ]; then
  . $HOME/.asdf/asdf.sh
  fpath=(${ASDF_DIR}/completions $fpath)
fi

if which direnv > /dev/null; then
  eval "$(direnv hook zsh)"
fi

if [ -d $HOME/.poetry ]; then
  export PATH=$HOME/.poetry/bin:$PATH
fi

if [ -d $HOME/.cargo ]; then
  export PATH=$HOME/.cargo/bin:$PATH
fi

if [ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ]; then
  source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"
fi

# stack
if [ -d $HOME/.local/bin ]; then
  export PATH=$HOME/.local/bin:$PATH
fi

# flutter
if [ -d $HOME/flutter/bin ]; then
  export PATH=$HOME/flutter/bin:$PATH
fi
if [ -d $HOME/.pub-cache/bin ]; then
  export PATH=$HOME/.pub-cache/bin:$PATH
fi

# deno
if [ -d $HOME/.deno/bin ]; then
  export PATH=$HOME/.deno/bin:$PATH
fi

# OCaml
if executable opam; then
  eval $(opam env)
fi

# basis
setopt ignoreeof  # Ignore Ctrl-D
unsetopt list_beep  # 補完時にベルを鳴らさない
setopt nonomatch  # glob の暴発を防ぐ

HISTSIZE=100000
SAVEHIST=100000
setopt share_history  # 即座に history を書き出す
setopt hist_ignore_dups
setopt hist_ignore_all_dups
bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# # 補完
#
# # ↓ 補完の表示強化
# zstyle ':completion:*' verbose yes
# zstyle ':completion:*' completer _expand _complete _match _prefix _approximate _list _history
# zstyle ':completion:*:messages' format '%F{YELLOW}%d'$DEFAULT
# zstyle ':completion:*:warnings' format '%F{RED}No matches for:''%F{YELLOW} %d'$DEFAULT
# zstyle ':completion:*:descriptions' format '%F{YELLOW}completing %B%d%b'$DEFAULT
# zstyle ':completion:*:options' description 'yes'
# zstyle ':completion:*:descriptions' format '%F{yellow}Completing %B%d%b%f'$DEFAULT
#
# zstyle ':completion:*:default' menu select=2  # 選択中の候補をハイライト
# export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
# zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}  # 補完時の色
#
# zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}'


# prompt
setopt prompt_subst
autoload -Uz colors
colors

PROMPT='%{$fg[blue]%}$(uname -m) %{$fg[green]%}$(basename $(pwd))%{${reset_color}%} %(?/%{$fg_bold[green]%}:)/%{$fg_bold[red]%}:()%{${reset_color}%}%(1j. %{$fg[red]%}(%j)%{$reset_color%}.) $ '

# fzf
export FZF_LEGACY_KEYBINDINGS=0
export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git/*'"
export FZF_FIND_FILE_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_DEFAULT_OPTS='--extended --ansi --multi --height 40% --reverse --bind=ctrl-u:page-up --bind=ctrl-d:page-down --bind=ctrl-z:toggle-all --reverse --height 40%'

fzf-select-history() {
    BUFFER=$(history -n -r 1 | fzf --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N fzf-select-history
bindkey '^r' fzf-select-history


# aliases
alias ls='ls -F -G'

g() {
  local src=$(ghq list | fzf --preview "ls -lTp $(ghq root)/{} | tail -n+2 | awk '{print \$9\"/\"\$6\"/\"\$7 \" \" \$10}'")
  if [ -n "$src" ]; then
    cd $(ghq root)/$src
  fi
}

# Read the local configurations.
if [ -r $HOME/.zsh/local.zsh ]; then
  . $HOME/.zsh/local.zsh
fi

# precmd
add_newline() {
  if [[ -z $PS1_NEWLINE_LOGIN ]]; then
    PS1_NEWLINE_LOGIN=true
  else
    printf '\n'
  fi
}

precmd() {
  # add_newline
}

eval "$(sheldon source)"
eval "$(starship init zsh)"

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

