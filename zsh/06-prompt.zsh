#! /usr/bin/zsh

local FMT_BRANCH="%F{9}(%s:%F{7}%r%F{9}) %F{5}%b%F{199}%u%c"
local FMT_ACTION="(%F{3}%a%f)"
local FMT_PATH="%F{1}%R%F{2}/%S%f"

# setprompt() {
#   local USER="%(#.%F{1}.%F{3})%n%f"
#   local HOST="%F{2}%M%f"
#   local PWD="%F{7}$($HOME/.dotfiles/bin/rzsh_path)%f"
#   local TTY="%F{4}%y%f"
#   local EXIT="%(?..%F{202}%?%f)"
#   local PRMPT="${USER}@$HOST:${TTY}: ${PWD}
# ${EXIT} %F{202}›%f "

setprompt() {
  local USER="%(#.%F{1}.%F{1})%n%f"
  local HOST="%F{2}%M%f"
  local PWD="%F{7}$($HOME/.dotfiles/bin/rzsh_path)%f"
  local TTY="%F{4}%y%f"
  local EXIT="%(?..%F{202}%?%f)"
  local PRMPT="${USER}@$HOST:${TTY}: ${PWD}
${EXIT} %F{202}›%f "

  if [[ "${vcs_info_msg_0_}" == "" ]]; then
    PROMPT="$PRMPT"
  else
    PROMPT="${vcs_info_msg_0_}
$PRMPT"
  fi
}

# {{{ Title stuffs
precmd() {

  vcs_info
  setprompt

  case $TERM in
    rxvt-256color | screen-256color ) 
      print -Pn "\e]0;%n@%m: %~\a" ;;
  esac
}

preexec() {
  case $TERM in
    rxvt-256color | screen-256color )
      print -Pn "\e]0;$1\a" ;;
  esac
} # }}}

# vim: set ft=zsh :
