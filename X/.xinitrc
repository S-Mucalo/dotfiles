#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources


# this makes it work in Ubuntu
#xhost +
## you might need to append the TTY you are working on
#xinit

#wmname LG3D
exec i3

# Remap caps lock to left control. This is not strictly speaking
# exwm related, but it's handy
#setxkbmap -option 'ctrl:no caps'

# Set fallback cursor
#xsetroot -cursor_name left_ptr

# If Emacs is started in server mode, `emacsclient` is a convenient way to edit
# files in place (used by e.g. `git commit`)
#export VISUAL=emacsclient
#export EDITOR="$VISUAL"

# Finally launch emacs and enable exwm
#exec dbus-launch --exit-with-session emacs

# xautolock -time 30 -locker "sudo pm-suspend" &
# dropboxd &
# xbindkeys &
# xcompmgr -cF &
# nm-applet --sm-disable &
# udiskie --tray &
# if [ -s ~/.Xmodmap ]; then
#     xmodmap ~/.Xmodmap
# fi
# vboxclient-all &

# exec $1

# DEFAULT_SESSION=awesome

# case $1 in
# LXDE)
#   exec startlxde
#   ;;
# qtile)
#   exec qtile
#   ;;
# xmonad)
#   exec xmonad
#   ;;
# *)
#   exec $DEFAULT_SESSION
#   ;;
# esac
#exec awesome
#exec startlxde
# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
