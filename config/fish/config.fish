# fisher
if not functions -q fisher
  set -q XDG_CONFIG_HOME; or set -x XDG_CONFIG_HOME $HOME/.config
  curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
end

# path
set -x GOPATH $HOME/repos
set -x PATH $GOPATH/bin $PATH
if test -d $HOME/.cargo/bin
  set -x PATH $HOME/.cargo/bin $PATH
end


# fzf
set -x FZF_LEGACY_KEYBINDINGS 0
set -x FZF_DEFAULT_COMMAND "rg --files --hidden --follow --glob '!.git/*'"
set -x FZF_FIND_FILE_COMMAND $FZF_DEFAULT_COMMAND
set -x FZF_DEFAULT_OPTS '--extended --ansi --multi --height 40% --reverse --bind=ctrl-u:page-up --bind=ctrl-d:page-down --bind=ctrl-z:toggle-all --reverse --height 40%'

### UI

# git status
set __fish_git_prompt_show_informative_status
set __fish_git_prompt_showcolorhints
set __fish_git_prompt_showupstream "informative"
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red
