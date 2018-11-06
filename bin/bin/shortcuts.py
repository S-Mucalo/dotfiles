#! /usr/bin/env python3

from pathlib import Path
import os
import sys
from re import sub
from re import compile

import os
import pwd
username = pwd.getpwuid(os.getuid())[0]

folders = (("h", "~"),
           ("d", "~/Documents"),
           ("D", "~/Downloads"),
           ("pp", "~/Pictures"),
           ("N", "~/Nextcloud"),
           ("vv", "~/Videos"),
           ("m", "~/Music"),
           ("b", "~/Books"),
           ("c", "~/dotfiles/"),
           ("r", "/"),
           ("M", "/run/media/{}".format(username)),
           ("cf", "~/.config"))

configs = (("cfz", "~/.zshrc"),
           ("cfe", "~/.emacs.d/config.org"),
           ("cfr", "~/.config/ranger/rc.conf"),
           ("cfq", "~/.config/qutebrowser/config.py"),
           ("cft", "~/.tmux.conf"),
           ("cfd", "~/.Xresources"),
           ("cfi", "~/.config/i3/config"))

quteshortcuts = ""
rangershortcuts = ""
zshshortcuts = ""
home = Path.home()

rangerlocation = Path.joinpath(home, ".config/ranger/rc.conf")
zshlocation = Path.joinpath(home, ".config/zsh/01-aliases.zsh")
qutelocation = Path.joinpath(home, ".config/qutebrowser/config.py")


# These are the labels that demarcate where the shortcuts
# go in the config files.
beg = "# SHORTCUT START\n"
end = "# SHORTCUT END"

#First we open the list of folder shortcuts and go down each line adding each in the required syntax to each of the three configs:

# with open(home+".config/Scripts/folders") as fold:
#     for line in csv.reader(fold, dialect="excel-tab"):
for fold in folders:
    # Adds the ranger go, tab, move and yank commands:
    rangershortcuts += ('map g{} cd {}\\n'.format(fold[0], fold[1]))
    rangershortcuts += ('map t{} tab_new {}\\n'.format(fold[0], fold[1]))
    rangershortcuts += ('map m{} shell mv %s {}\\n'.format(fold[0], fold[1]))
    rangershortcuts += ('map Y{} shell cp -r %s {}\n'.format(fold[0], fold[1]))
    # Adds the bashshortcuts shortcuts:
    zshshortcuts += ('alias {}="cd {} && ls -a"\n'.format(fold[0], fold[1]))
    # qutebrowser shortcuts:
    quteshortcuts += ('config.bind(";{}", "set downloads.location.directory {} ;; hint links download"\n')

# Goes thru the config file file and adds the shortcuts to both zshshortcuts and ranger.

for conf in configs:
    zshshortcuts += ('alias {}="emacsclient -nc -a emacs {}"\n'.format(conf[0], conf[1]))
    rangershortcuts += ('map {} shell emacsclient -nc -a emacs {}\n'.format(conf[0], conf[1]))


def replaceInMarkers(text, shortcuts):
    markers = compile(beg + "(.|\s)*" + end)
    replacement = beg + shortcuts + end
    return sub(markers, replacement, text)



def writeShortcuts(location, shortcuts):
    with open(location, "r+") as input:
        final = ""
        final += input.read()
        final = replaceInMarkers(final, shortcuts)
        input.seek(0)
        input.write(final)
        input.truncate()

def delShortcuts(location):
    with open(location, "r+") as input:
        markers = compile(beg+"(.|\s)*"+end)
        replacement = beg + end
        final = input.read()
        final = sub(markers, replacement, final)
        input.seek(0)
        input.write(final)
        input.truncate()


if __name__ == '__main__':

    if len(sys.argv) < 2:
        writeShortcuts(rangerlocation, rangershortcuts)
        writeShortcuts(zshlocation, zshshortcuts)
        writeShortcuts(qutelocation, quteshortcuts)
    else:
        if sys.argv[1] == "--remove-shortcuts":
            delShortcuts(rangerlocation)
            delShortcuts(zshlocation)
            delShortcuts(qutelocation)
        else:
            print("Usage:\n{0} \t\t\t write shortcuts\n{0} --remove-shortcuts\t remove-shortcuts".format(os.path.basename(sys.argv[0]))
)
