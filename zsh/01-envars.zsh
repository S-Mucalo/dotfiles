## ZSH Environment Variables
## ~/.config/zsh

export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE=~/.dotfiles/zsh/history.log
export DISPLAY=:0

export SHELL='/bin/zsh'

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient"

export PATH="/usr/local/sbin:\
/usr/local/bin:\
/usr/bin:\
/opt/opencascade/bin:\
/usr/bin/vendor_perl:\
/usr/bin/core_perl:\
$HOME/.dotfiles/bin:"

# Less
export LESSOPEN='| /usr/bin/highlight -O ansi %s'
export LESS='-A$-R$-g$-i$-m$-s'

# vim: set ft=zsh
