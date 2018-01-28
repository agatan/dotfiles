if [ -d $HOME/.pyenv/bin ]; then
    export PATH=$HOME/.pyenv/bin:$PATH
fi

if ! exists pyenv; then
    return
fi
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
