#!/bin/bash

# Create .config directory if needed
if [ ! -d ~/.config ]
  then mkdir ~/.config
fi

declare -a links=(.config/awesome .Xresources .zshrc .ncmpcpp)

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
ln -s $HOME/.dotfiles/X/XkeymapUS $HOME/.XkeymapUS
ln -s $HOME/.dotfiles/X/XkeymapDE $HOME/.XkeymapDE

# zsh
ln -s $HOME/.dotfiles/zsh/zshrc $HOME/.zshrc

# ncmpcpp
ln -s $HOME/.dotfiles/ncmpcpp/ $HOME/.ncmpcpp

# ranger
ln -s $HOME/.dotfiles/ranger/ $HOME/.config/ranger

# browser
ln -s $HOME/.dotfiles/browser/pentadactylrc $HOME/.pentadactylrc

echo "You'll have to symlink the userChrome.css file manually if you want to use it."
echo "If you want to setup mail, check the .dotfiles/mail directory."
