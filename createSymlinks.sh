#!/bin/bash

# Create .config directory if needed
if [ ! -d ~/.config ]
  then mkdir ~/.config
fi

declare -a links=(.config/awesome .Xresources .xbindkeysrc .ncmpcpp
    .xinitrc .emacs.d/init.el .emacs.d/custom.el .bashrc .gitconfig
    .keysnail.js .config/ranger .mpd)

# If files already exist create backups
for i in ${links[*]}
do
  if [ -e $HOME/$i ]
    then mv ~/$i ~/$i.backup
  fi
done

# Awesome
ln -s $HOME/.dotfiles/awesome/ $HOME/.config/awesome

# zsh
ln -s $HOME/.dotfiles/zsh/zshrc $HOME/.zshrc

# Emacs
ln -s $HOME/.dotfiles/emacs/init.el $HOME/.emacs.d/init.el
ln -s $HOME/.dotfiles/emacs/custom.el $HOME/.emacs.d/custom.el

# bash
ln -s $HOME/.dotfiles/bash/bashrc $HOME/.bashrc

# X
ln -s $HOME/.dotfiles/X/xinitrc $HOME/.xinitrc
ln -s $HOME/.dotfiles/X/Xresources $HOME/.Xresources
ln -s $HOME/.dotfiles/X/xbindkeysrc $HOME/.xbindkeysrc

# mpd
ln -s $HOME/.dotfiles/mpd/ $HOME/.mpd
 
# ncmpcpp
ln -s $HOME/.dotfiles/ncmpcpp/ $HOME/.ncmpcpp

# ranger
ln -s $HOME/.dotfiles/ranger/ $HOME/.config/ranger

# git
ln -s $HOME/.dotfiles/git/gitconfig $HOME/.gitconfig

# browser
ln -s $HOME/.dotfiles/browser/keysnail.js $HOME/.keysnail.js

echo "If you want to setup mail, check the .dotfiles/mail directory."
