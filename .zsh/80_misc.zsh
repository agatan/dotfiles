zstyle ':completion:*:default' menu select=2

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' verbose yes
zstyle ':completion:*' use-cache true

zstyle ':completion:*' ignore-parents parent pwd ..
