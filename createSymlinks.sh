#!/bin/bash

# Create .config directory if needed
if [ ! -d ~/.config ]
  then mkdir ~/.config
fi

declare -a links=(.config/awesome .Xresources .ncmpcpp)

# If files already exist create backups
for i in ${links[*]}
do
  if [ -e $HOME/$i ]
    then mv ~/$i ~/$i.backup
  fi
done

# Awesome
ln -s $HOME/.dotfiles/awesome/ $HOME/.config/awesome

# X
ln -s $HOME/.dotfiles/X/xinitrc $HOME/.xinitrc
ln -s $HOME/.dotfiles/X/Xresources $HOME/.Xresources

# ncmpcpp
ln -s $HOME/.dotfiles/ncmpcpp/ $HOME/.ncmpcpp

# ranger
ln -s $HOME/.dotfiles/ranger/ $HOME/.config/ranger

echo "If you want to setup mail, check the .dotfiles/mail directory."
