#! /usr/bin/zsh

## ZSH Environment Variables
## ~/.config/zsh

export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE=~/.dotfiles/zsh/history.log
# export DISPLAY=:0

export SHELL='/bin/zsh'

export ALTERNATE_EDITOR=""
export EDITOR="emacs"

export BROWSER="qutebrowser"
export TERMINAL="urxvt"

# Paths
export PATH=$PATH:$HOME/.dotfiles/bin
export PATH=$PATH:$HOME/.local/bin
export PYTHONPATH=$PYTHONPATH:$HOME/.dotfiles/bin
export PYTHON2PATH=$PYTHON2PATH:$HOME/.dotfiles/bin

# Less
LESS="$LESS -R -I -M"
export LESS
LESSOPEN="| colorize_via_pygmentize.sh %s"
export LESSOPEN

# PETSc
export PETSC_DIR=$HOME/petsc
export PETSC_ARCH=arch-linux2-gcc-real
export PYTHONPATH=$PYTHONPATH:$PETSC_DIR/$PETSC_ARCH/lib
export PYTHON2PATH=$PYTHON2PATH:$PETSC_DIR/$PETSC_ARCH/lib

# Python virtualenv
if [ -f /usr/bin/virtualenvwrapper.sh ]; then
    source /usr/bin/virtualenvwrapper.sh
elif [ -f /usr/share/virtualenvwrapper/virtualenvwrapper.sh ]; then
    source /usr/share/virtualenvwrapper/virtualenvwrapper.sh
fi
export WORKON_HOME=$HOME/.virtualenvs

# vim: set ft=zsh
