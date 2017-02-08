# プロンプト
# 2行表示
PROMPT="%F{cyan}[%n@%md]%f%(1j,%{${fg[red]}%}(%j)%{${reset_color}%},)
%{${fg[yellow]}%}%~%{${reset_color}%}
%(?.%B%F{green}.%B%F{red})%(?!:) $ !:( $ )%f%b"

########################################
# vcs_info
autoload -Uz vcs_info
autoload -Uz add-zsh-hook

zstyle ':vcs_info:*' formats '%F{green}(%s)-[%b]%f'
zstyle ':vcs_info:*' actionformats '%F{red}(%s)-[%b|%a]%f'

function _update_vcs_info_msg() {
    LANG=en_US.UTF-8 vcs_info
    RPROMPT="${vcs_info_msg_0_}"
}
add-zsh-hook precmd _update_vcs_info_msg
