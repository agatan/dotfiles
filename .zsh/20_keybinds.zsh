bindkey -d
bindkey -e

bindkey '^x^g' fzf-ghq-cd

bindkey '^r' fzf-history

if exists 'history-substring-search-up'; then
    bindkey '^P' history-substring-search-up
fi

if exists 'history-substring-search-down'; then
    bindkey '^N' history-substring-search-down
fi
