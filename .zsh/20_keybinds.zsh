bindkey '^x^g' fzf-ghq-cd

bindkey '^r' fzf-history

if exists 'history-substring-search-up'; then
    bindkey '^P' history-substring-search-up
fi

if exists 'history-substring-search-down'; then
    bindkey '^N' history-substring-search-down
fi

bindkey -s "^Z" "^Ufg^M"

do_enter() {
    if [[ -n $BUFFER ]]; then
        zle accept-line
        return $status
    fi
    echo
    if [[ -d .git ]] && [[ -n "$(git status --short)" ]]; then
        git status
    fi

    zle reset-prompt
}
zle -N do_enter
bindkey '^m' do_enter
