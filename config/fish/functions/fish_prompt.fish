function __count_bg_jobs
  jobs -p | wc -l
end


function fish_prompt
  set -l laststatus $status

  set -l jobcount (jobs -p | wc -l)
  set_color cyan
  printf '[%s@%s]' $USER (prompt_hostname)
  if [ "$jobcount" -ne 0 ]
    set_color red
    printf ' (%d)' $jobcount
  end
  printf '\n'

  set_color yellow
  printf '%s\n' (prompt_pwd)

  if [ $laststatus -eq 0 ]
    set_color green
    printf ':) $ '
  else
    set_color red
    printf ':( $ '
  end
end
