if ! exists rbenv; then
    return
fi
export PATH="~/.rbenv/bin:$PATH"
eval "$(~/.rbenv/bin/rbenv init -)"
