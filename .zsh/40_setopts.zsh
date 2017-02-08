# Replace 'cd -' with 'cd +'
setopt pushd_minus
# Ignore duplicates to add to pushd
setopt pushd_ignore_dups
# pushd no arg => push $HOME
setopt pushd_to_home

# spell check
setopt correct
setopt correct_all

setopt interactive_comments
setopt print_eight_bit

# No Beeps
setopt no_beep
setopt no_list_beep
setopt no_hist_beep

# Show exit status if prev command fails
setopt print_exit_value

# Confirm if 'rm *'
setopt rm_star_wait

setopt no_flow_control

# add '/' to ends of pathes when generating path by glob
setopt mark_dirs

# Use extended glob pattern
setopt extended_glob

# history
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_save_nodups
# ignore if input starts with spaces
setopt hist_ignore_space
setopt hist_reduce_blanks
