function g
  set -l ghqroot (ghq root)
  find $ghqroot/github.com -mindepth 1 -maxdepth 2 -type d | \
    string replace $ghqroot/github.com/ '' | \
    fzf --query "$argv" | \
    read -l result
  if [ -d $ghqroot/github.com/$result ]
    builtin cd $ghqroot/github.com/$result
    commandline -f repaint
  end
end

