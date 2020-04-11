if which vim >/dev/null; then
  export EDITOR=vim
elif which vi >/dev/null; then
  export EDITOR=vi
elif which emacs >/dev/null; then
  export EDITOR=emacs
fi

export GOPATH=$HOME/repos
export PATH=$GOPATH/bin:$PATH

if [ -d $HOME/.anyenv ]; then
  export PATH=$HOME/.anyenv/bin:$PATH
  eval "$(anyenv init - zsh)"
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


# basis
setopt ignoreeof  # Ignore Ctrl-D
unsetopt LIST_BEEP  # 補完時にベルを鳴らさない

bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line


# 補完
autoload -U compinit
compinit -u

# ↓ 補完の表示強化
zstyle ':completion:*' verbose yes
zstyle ':completion:*' completer _expand _complete _match _prefix _approximate _list _history
zstyle ':completion:*:messages' format '%F{YELLOW}%d'$DEFAULT
zstyle ':completion:*:warnings' format '%F{RED}No matches for:''%F{YELLOW} %d'$DEFAULT
zstyle ':completion:*:descriptions' format '%F{YELLOW}completing %B%d%b'$DEFAULT
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:descriptions' format '%F{yellow}Completing %B%d%b%f'$DEFAULT

zstyle ':completion:*:default' menu select=2  # 選択中の候補をハイライト
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}  # 補完時の色


# prompt
setopt prompt_subst
autoload -Uz colors
colors
PROMPT="%{$fg[green]%}$(basename $(pwd)) %(?/%{$fg_bold[green]%}:)/%{$fg_bold[red]%}:()%{${reset_color}%} $ "

git-current-status() {
  if [ ! -d ".git" ]; then
    return
  fi
  local branch_name=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
  local st=$(git status 2> /dev/null)
  if [[ -n $(echo "$st" | grep "^nothing to") ]]; then
    # 全てcommitされてクリーンな状態
    branch_status="%{$fg[green]%}"
  elif [[ -n $(echo "$st" | grep "^Untracked files") ]]; then
    # gitに管理されていないファイルがある状態
    branch_status="%{$fg[red]%}?"
  elif [[ -n $(echo "$st" | grep "^Changes not staged for commit") ]]; then
    # git addされていないファイルがある状態
    branch_status="%{$fg[red]%}+"
  elif [[ -n $(echo "$st" | grep "^Changes to be committed") ]]; then
    # git commitされていないファイルがある状態
    branch_status="%{$fg[yellow]%}!"
  elif [[ -n $(echo "$st" | grep "^rebase in progress") ]]; then
    # コンフリクトが起こった状態
    echo "%{$fg[red]%}!(no branch)%{${reset_color}%}"
    return
  else
    # 上記以外の状態の場合は青色で表示させる
    branch_status="%{$fg[blue]%}"
  fi
  # ブランチ名を色付きで表示する
  echo "${branch_status}[$branch_name]%{${reset_color}%}"
}
RPROMPT='$(git-current-status)'


# fzf
export FZF_LEGACY_KEYBINDINGS=0
export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git/*'"
export FZF_FIND_FILE_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_DEFAULT_OPTS='--extended --ansi --multi --height 40% --reverse --bind=ctrl-u:page-up --bind=ctrl-d:page-down --bind=ctrl-z:toggle-all --reverse --height 40%'


# aliases
alias ls='ls -F -G'
alias rm='rm -i'
alias ec=envchain

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
