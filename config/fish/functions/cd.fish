function cd
  commandline | read -l buffer
  if [ (count $argv) -ne 0 ]
    set -l to $argv[1]
    if [ -d $to ]
      builtin cd $to
      return 0
    end
  end
  z -l | awk '{ print $2 }' | fzf --query "$argv" | read -l result
  builtin cd $result
  commandline -f repaint
end

