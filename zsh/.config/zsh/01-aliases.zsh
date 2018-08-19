#! /usr/bin/zsh

alias hdmi='xrandr --output HDMI1 --auto --left-of LVDS'
alias pres='xrandr --fb 1366x768 --output DP2 --mode 1024x768 --panning 1366x0'
alias mon='xrandr --output DP2 --mode 1680x1050 --left-of eDP1'
alias upg='aurman -Syua --noconfirm'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias emd='emacsclient -nc'
alias em='emacs'
alias emc='emacsclient -t'
alias emt='emacs -nw'
alias killemnow='emacsclient -e "(kill-emacs)"'
alias killem="emacsclient -e '(client-save-kill-emacs)'"
alias shu='systemctl poweroff'
alias re='systemctl reboot'
alias sus='systemctl suspend'
alias ins='aurman -S'
alias unins='aurman -Rns'
alias search='aurman -Ss'
alias clean='sudo pacman -Rns $(pkg-list_true_orphans)'
alias pss='ps -a -c -o pid,command -x'
alias gen_ctags='find . -name "*.[chCH]" -print | etags -'

alias cless="less --no-lessopen"
alias cat="colorize_via_pygmentize.sh"
alias ccat="/usr/bin/env cat"

# ranger
alias r.ranger='SHELL=$HOME/bin/r.shell ranger'

# rsync
alias rsync-copy="rsync -avz --progress -h"
alias rsync-move="rsync -avz --progress -h --remove-source-files"
alias rsync-update="rsync -avzu --progress -h"
alias rsync-synchronize="rsync -avzu --delete --progress -h"

# tmux
alias ta='tmux attach -t'
alias tad='tmux attach -d -t'
alias ts='tmux new-session -s'
alias tl='tmux list-sessions'
alias tksv='tmux kill-server'
alias tkss='tmux kill-session -t'

# grep
grephistory()
{
    grep $1 $HOME/.zsh_hist
}
alias his='grephistory'

# SHORTCUT START
alias h="cd ~ && ls -a"
alias d="cd ~/Documents && ls -a"
alias D="cd ~/Downloads && ls -a"
alias pp="cd ~/Pictures && ls -a"
alias vv="cd ~/Videos && ls -a"
alias m="cd ~/Music && ls -a"
alias b="cd ~/Books && ls -a"
alias c="cd ~/.dotfiles/ && ls -a"
alias r="cd / && ls -a"
alias cf="cd ~/.config && ls -a"
alias cfz="emacsclient -nc -a \"\" ~/.zshrc"
alias cfe="emacsclient -nc -a \"\" ~/.emacs.d/init.el"
alias cfr="emacsclient -nc -a \"\" ~/.config/ranger/rc.conf"
alias cfq="emacsclient -nc -a \"\" ~/.config/qutebrowser/config.py"
alias cft="emacsclient -nc -a \"\" ~/.tmux.conf/"
alias cfd="emacsclient -nc -a \"\" ~/.Xresources"
alias cfi="emacsclient -nc -a \"\" ~/.config/i3/config"
# SHORTCUT END
