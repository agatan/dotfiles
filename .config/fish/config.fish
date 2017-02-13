####################
# My Settings
#
set -U fish_user_paths /usr/local/bin $fish_user_paths
set -U fish_user_paths /usr/local/sbin $fish_user_paths

# Golang
set -x GOPATH $HOME/repos
set -U fish_user_paths $fish_user_paths $GOPATH/bin

# anyenv
set -U fish_user_paths $HOME/.anyenv/bin $fish_user_paths
eval (anyenv init -)

