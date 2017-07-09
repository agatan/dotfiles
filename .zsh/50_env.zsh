if exists nodenv; then
    eval "$(nodenv init -)"
fi

if is_linux; then
    export XDG_CONFIG_HOME=$HOME/.config
fi
