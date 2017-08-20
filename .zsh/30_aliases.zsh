alias cp='cp -i'
alias mv='mv -i'

alias mkdir='mkdir -p'
alias ls='ls -G -F'
alias lla='ls -lAF'
alias ll='ls -lF'
alias la='ls -AF'

alias ..='cd ..'

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

# C で標準出力をクリップボードにコピーする
# mollifier delta blog : http://mollifier.hatenablog.com/entry/20100317/p1
if exists pbcopy; then
    # Mac
    alias -g CP='| pbcopy'
    alias -g CC='| tee /dev/tty | pbcopy'
elif exists xsel; then
    # Linux
    alias -g CP='| xsel --input --clipboard'
elif exists putclip; then
    # Cygwin
    alias -g CP='| putclip'
fi

alias -g N=" >/dev/null 2>&1"
alias -g N1=" >/dev/null"
alias -g N2=" 2>/dev/null"
alias -g H='| head'
alias -g T='| tail'
alias -g J='| jq . -C'
alias -g L='| less'
if exists fzf; then
    alias -g F='| fzf'
fi

if [ -n $ENHANCD_FILTER ]; then
    alias g='cd -G'
fi

mru() {
    local -a f1

    f1=(
    ~/.vim_mru_files(N)
    )

    # parse arguments
    local opt action
    action=echo
    for opt in $@
    do
        case $opt in
            '-a'|'--action')
                if [[ -z "$2" ]] || [[ "$2" =~ "^-+" ]]; then
                    echo "mru: option requires an argument -- $1" 1>&2
                    return 1
                fi
                action="$2"
                shift 2
                ;;
            -*)
                echo "mru: illegal option -- '$(echo $1 | sed 's/^-*//')'" 1>&2
                return 1
                ;;
            *)
                ;;
        esac
    done

    if [[ $#f1 -eq 0 ]]; then
        echo "There are no mru files." >&2
        return 1
    fi

    local cmd query key files
    while cmd="$(
        cat <$f1 \
            | grep -v '^#' | awk '!a[$0]++' \
            | fzf --ansi --multi --prompt='MRU> ' \
                  --query="${query}" \
                  --print-query \
                  --expect=ctrl-v,ctrl-l \
    )"; do
        query=$(head -1 <<< "$cmd")
        key=$(head -2 <<< "$cmd" | tail -1)
        files="$(sed '1,2d;/^$/d' <<< "$cmd")"
        case "$key" in
            ctrl-v)
                vim -p ${(@f)files} > /dev/tty < /dev/tty
                return
                ;;
            ctrl-l)
                arr=("${(@f)files}")
                if [[ -d ${arr[1]} ]]; then
                    ls -l ${(@f)files} < /dev/tty | less > /dev/tty
                else
                    less ${(@f)files} < /dev/tty > /dev/tty
                fi
                ;;
            *)
                eval "$action ${(@f)files}" > /dev/tty < /dev/tty
                break
                ;;
        esac
    done
}
alias -g FROM=mru
alias v='mru --action "vim -p"'

if exists 'fzf'; then
  alias ff='nvim -c Files'
fi
