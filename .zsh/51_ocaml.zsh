# execute only if opam command exists
if ! exists 'opam'; then
    return
fi

# OPAM configuration
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
eval `opam config env`

#ocamlspot
export OCAMLPARAM="_,bin-annot=1"
export OPAMKEEPBUILDDIR=1
