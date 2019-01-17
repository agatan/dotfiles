function g
  set -l ghqroot (ghq root)
  find $ghqroot/github.com/wantedly $ghqroot/github.com/agatan -maxdepth 1 -type d | \
    string replace $ghqroot/github.com/ '' | \
    fzf --query "$argv" | \
    read -l result
  builtin cd $ghqroot/github.com/$result
  commandline -f repaint
end

