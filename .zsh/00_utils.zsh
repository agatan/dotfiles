exists() {
    type "${1:?too few arguments}" &>/dev/null
}

is_git_repo() {
    git rev-parse --is-inside-work-tree &>/dev/null
    return $status
}

is_tmux_running() {
    [[ -n "$TMUX" ]]
}

is_interactive_shell() {
    [[ -n "$PS1" ]]
}

is_ssh_running() {
    [ ! -z "$SSH_CONNECTION" ]
}

ostype() {
    echo ${(L):-$(uname)}
}

os_detect() {
    export PLATFORM
    case "$(ostype)" in
        *'linux'*) PLATFORM='linux';;
        *'darwin'*) PLATFORM='osx';;
        *) PLATFORM='unknown';;
    esac
}

is_osx() {
    os_detect
    [[ $PLATFORM == 'osx' ]]
}

is_linux() {
    os_detect
    [[ $PLATFORM == "linux" ]]
}

getos() {
    os_detect
    echo $PLATFORM
}
