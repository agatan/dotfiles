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
alias -g J='| jq .'
alias -g L='| less'

if [ -n $ENHANCD_FILTER ]; then
    alias cdg='cd -G'
fi
