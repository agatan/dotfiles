if [ -d $HOME/.anyenv ]; then
  export PATH=$HOME/.anyenv/bin:$PATH
  eval "$(anyenv init -)"
fi

if exists nodenv; then
    eval "$(nodenv init -)"
fi

if exists npm; then
  export PATH=$(npm bin)/$PATH
fi

if is_linux; then
    export XDG_CONFIG_HOME=$HOME/.config
fi

export LESS="--no-init --LONG-PROMPT --RAW-CONTROL-CHARS --quit-if-one-screen --jump-target=5 --ignore-case --shift=4"
## See terminfo(5)
export LESS_TERMCAP_mb=`tput bold; tput setaf 1`		## turn on bold
export LESS_TERMCAP_md=`tput bold; tput setaf 6`		## turn on blink
export LESS_TERMCAP_me=`tput sgr0`				## turn off all
export LESS_TERMCAP_so=`tput bold; tput setaf 3; tput setab 4`  ## begin standout mode
export LESS_TERMCAP_se=`tput sgr0`				## exit standout mode
export LESS_TERMCAP_us=`tput bold; tput setaf 3`		## begin underline mode
export LESS_TERMCAP_ue=`tput sgr0`				## exit underline mode
