autoload -Uz run-help
autoload -Uz colors && colors
autoload -Uz compinit && compinit

export LANGUAGE="en_US.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

export EDITOR=vim

export PAGER=less
export LESS='-R -f -i'
