function tmux_automatically_attach_session()
{
    if is_tmux_running; then
        echo "${fg_bold[red]} _____ __  __ _   ___  __ ${reset_color}"
        echo "${fg_bold[red]}|_   _|  \/  | | | \ \/ / ${reset_color}"
        echo "${fg_bold[red]}  | | | |\/| | | | |\  /  ${reset_color}"
        echo "${fg_bold[red]}  | | | |  | | |_| |/  \  ${reset_color}"
        echo "${fg_bold[red]}  |_| |_|  |_|\___//_/\_\ ${reset_color}"
    else
        if is_interactive_shell && ! is_ssh_running; then
            if ! exists 'tmux'; then
                echo 'Error: tmux command not found' 1>&2
                return 1
            fi

            if tmux has-session >/dev/null 2>&1 && tmux list-sessions | grep -qE '.*]$'; then
                tmux list-sessions
                echo -n 'Tmux: attach? (y/N/num)'
                read
                if [[ "$REPLY" =~ ^[Yy]$ ]] || [[ "$REPLY" == '' ]]; then
                    tmux attach-session
                    if [ $? -eq 0 ];
                        echo "$(tmux -V) attached"
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[0-9]+$ ]]; then
                    tmux attach -t "$REPLY"
                    if [ $? -eq 0]; then
                        echo "$(tmux -V) attached"
                        return 0
                    fi
                fi
                if is_osx && exists 'reattach-to-user-namespace'; then
                    # on OS X force tmux's default command
                    # to spawn a shell in the user's namespace
                    tmux_config=$(cat $HOME/.tmux.conf <(echo 'set-option -g default-command "reattach-to-user-namespace -l $SHELL"'))
                    tmux -f <(echo "$tmux_config") new-session && echo "$(tmux -V) created new session supported OS X"
                else
                    tmux new-session && echo "tmux created new session"
                fi
            fi
        fi
}
tmux_automatically_attach_session
