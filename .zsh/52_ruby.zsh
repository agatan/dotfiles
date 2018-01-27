if [ -d $HOME/.rbenv/bin ]; then
    export PATH=$HOME/.rbenv/bin:$PATH
fi

if ! exists rbenv; then
    return
fi
eval "$(rbenv init -)"
