#! /usr/bin/zsh


# # set formats
# # %b - branchname
# # %u - unstagedstr (see below)
# # %c - stagedstr (see below)
# # %a - action (e.g. rebase-i)
# # %R - repository path
# # %S - path in the repository
# # (%s:%r) %b %u%c
# local FMT_BRANCH="%{$fg[red]%}(%s:%{$fg[white]%}%r%{$fg[red]%}) %{$fg[magenta]%}%b%{%F{199}%}%u%c"
# local FMT_ACTION="(%F{3}%a%f)"
# local FMT_PATH="%F{1}%R%F{2}/%S%f"

# setprompt() {
#   local USER="%(#.%F{red}.%F{yellow})%n%f"
#   local HOST="%F{cyan}%M%f"
#   local PWD="$(pwd | sed  's:/home/[A-Za-z0-9]*:~:' | sed 's:/:%F{green}/%F{white}:g')%F{green}/%f"
#   local EXIT="%(?..%F{202}%?%f)"
#   local PRMPT="${USER}@$HOST ${PWD}"
#   local PRMPT2="${EXIT} %F{202}>%f" # ›

#   local _newline=$'\n'
#   local _lineup=$'\e[1A'
#   local _linedown=$'\e[1B'

#   if [ -z $VIRTUAL_ENV ]; then
#       local VIRT_ENV=""
#   else
#       local VIRT_ENV="%F{blue}(%f$(echo $VIRTUAL_ENV | sed 's:/.*.virtualenvs/::g')%F{blue})%f"
#   fi
#   PROMPT=$PRMPT${_newline}$PRMPT2
#   RPROMPT="%{${_lineup}%}${VIRT_ENV} ${vcs_info_msg_0_}%{${_linedown}%}"
# }

# # {{{ Title stuffs
# precmd() {
#   vcs_info
#   setprompt
# }

# preexec() {
# }
powerline-daemon -q
. /usr/lib/python3.8/site-packages/powerline/bindings/zsh/powerline.zsh
# vim: set ft=zsh :
