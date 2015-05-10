# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme coffeeandcode

# All built-in plugins can be found at ~/.oh-my-fish/plugins/
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Enable plugins by adding their name separated by a space to the line below.
set fish_plugins theme

# Path to your custom folder (default path is ~/.oh-my-fish/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

set -x PATH /usr/local/sbin /usr/local/bin $PATH


#############################################
# for softwares
#############################################

# for atom
set -x ATOM_PATH /opt/homebrew-cask/Caskroom/atom/latest


#############################################
# for languages
#############################################

# for rust
set -x RUST_SRC_PATH ~/rust/rust/src

# for golang
set -x GOPATH ~/go
set -x PATH $GOPATH/bin $PATH


# for haskell
set -x CABAL_HOME $HOME/.cabal
set -x PATH $CABAL_HOME/bin $PATH


# for ocaml ( not useful )
if [ -e ~/.opam/opam-init/init.fish ]
  source ~/.opam/opam-init/init.fish > /dev/null ^ /dev/null; or true
  eval (opam config env)
end

set -x OCAMLPARAM "_,bin-annot=1"
set -x OPAMKEEPBUILDDIR 1
