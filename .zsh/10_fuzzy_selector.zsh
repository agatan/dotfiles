export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git/*'"
export FZF_FIND_FILE_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_DEFAULT_OPTS='
--extended
--ansi
--multi
--height 40%
--reverse
--bind=ctrl-u:page-up
--bind=ctrl-d:page-down
--bind=ctrl-z:toggle-all
--reverse
--height 40%
'

fzf-ghq-cd() {
    local selected_dir=$(ghq list --full-path | fzf --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        BUFFER="cd $selected_dir"
        zle accept-line
    fi
}
zle -N fzf-ghq-cd

fzf-history() {
    BUFFER=`history -n 1 | awk '!a[$0]++' | fzf --tac --no-sort --query "$LBUFFER"`
    CURSOR=$#BUFFER
}
zle -N fzf-history

killp() {
    ps a | tail -n +2 | \
        fzf --query "$LBUFFER" | \
    cut -d' ' -f 1 |\
    while read pid
    do
        kill -KILL $pid
    done
}

# git

git-del-branch() {
    git branch -D $(git branch | fzf)
}

# godoc
fdoc() {
    local selected_dir=$(ghq list --full-path | fzf --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        if [ ! -n "$PAGER" ]; then
           PAGER=less
        fi
        godoc ${selected_dir} | $PAGER
    fi
}
