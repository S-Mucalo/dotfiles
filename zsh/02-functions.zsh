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

# {{{ Oneliners
goto() { [ -d "$1" ] && cd "$1" || cd "$(dirname "$1")"; }
cpf() { cp "$@" && goto "$_"; }
mvf() { mv "$@" && goto "$_"; }
mkf() { mkdir -p $1; cd $1 }
cdl() { cd $@; ls++ }
d() { ($1 &) }
zsh_stats() { history | awk '{print $2}' | sort | uniq -c | sort -rn | head }
du1() { du -h --max-depth=1 "$@" | sort -k 1,1hr -k 2,2f; }
epoch() { print $(( `echo $1 | cut -b 1-2` * 3600 + `echo $1 | cut -b 4-5` * 60 + `echo $1 | cut -b 7-8` )) }
findem() { find / -name "$1" 2>/dev/null; }
readem() { find / -name "$1" 2>/dev/null | xargs less; }
findit() { find . -name "$1" }
# }}}

# {{{ Create ZSH named directory
namedir() {
  echo "$1=$PWD ;  : ~$1" >> ~/.config/zsh/directories
  . ~/.config/zsh/directories
} # }}}

# {{{ Most used Commands
mostused() {
  sed -n 's/^\([a-z]*\) .*/\1/p' $HOME/.dotfiles/zsh/history.log |
  sort |
  uniq -c |
  sort -n -k1 |
  tail -25 |
  tac
} # }}}


# {{{ Rip Audio CDs to MP3
ripdatshit()
{
  echo "MP3 VBR quality setting: [0-9]"
  read $q
  mkdir $HOME/tmp/rip
  cd $HOME/tmp/rip
  cdparanoia -B
  for i in *.wav; do
    lame -V $q $i mp3/${i%.*.*}.mp3
  done
  echo "Tag mp3 files with Easytag? [y/n]"
  read $yn
  if [[ "$yn" == "y" ]]; then
    easytag $HOME/tmp/rip
  fi
} # }}}

# {{{ Create ISO from device or directory
mkiso()
{
  case $1 in
    /dev/*)
      dd if=$1 of=$2 ;;
    *)
      mkisofs -o $2 $1 ;;
  esac
} # }}}

# {{{ Setup empty github repo
mkgit() {
  mkdir $1
  cd $1
  git init
  touch README.markdown
  git add README.markdown
  git commit -m 'inital setup - automated'
  git remote add origin git@github.com:Zaidean/$1.git
  git push origin master
} # }}}

# {{{ Archiving - Compress/decompress various archive types with a single command
ark() {
  case $1 in

    e)
      case $2 in
        *.tar.bz2)   tar xvjf $2      ;;
        *.tar.gz)    tar xvzf $2      ;;
        *.bz2)       bunzip2 $2       ;;
        *.rar)       unrar x $2       ;;
        *.gz)        gunzip $2        ;;
        *.tar)       tar xvf $2       ;;
        *.tbz2)      tar xvjf $2      ;;
        *.tgz)       tar xvzf $2      ;;
        *.zip)       unzip $2         ;;
        *.Z)         uncompress $2    ;;
        *.7z)        7z x $2          ;;
        *)           echo "'$2' kann nicht mit >ark< entpackt werden" ;;
      esac ;;

    c)
      case $2 in
        *.tar.*)    arch=$2; shift 2;
                    tar cvf ${arch%.*} $@
                    case $arch in
                      *.gz)   gzip -9r ${arch%.*}   ;;
                      *.bz2)  bzip2 -9zv ${arch%.*} ;;
                    esac                                ;;
        *.rar)      shift; rar a -m5 -r $@; rar k $1    ;;
        *.zip)      shift; zip -9r $@                   ;;
        *.7z)       shift; 7z a -mx9 $@                 ;;
        *)          echo "Kein gültiger Archivtyp"      ;;
      esac ;;

    *)
      echo "WATT?" ;;

  esac
} # }}}


lessr() { cat $1 | less -R }
# }}}

# {{{ Quick Link saving for fast saves/recalls
addfile() {echo $2 >> /media/data/Filez/$1}
searchfile() {ls /media/data/Filez | grep $1}
getfile() {cat /media/data/Filez/$1 | xclip}
losefile() {rm /media/data/Filez/$1}
# }}}

# {{{ Functions for more comfortable use of DropboxCLI
# requires:
# dropbox-cli
# cush
# xclip
# ZSH named directory "~Dropbox"

dbheader() {
  echo -e " Dropbox - $(dropbox status)
======================================================"
}

dbshorturl() {
  url=$(cush -o -- $(dropbox puburl ~Dropbox/Public/$1))
  echo -n $url | xclip
  echo $url
}

dbup() {
  dbheader
  cp $1 /media/data/Dropbox/Public
  dbshorturl $1
}

dbls() {
  dbheader
  dropbox ls /media/data/Dropbox/$1
}

dburl() {
  dbheader
  dbshorturl $1
}
# }}}

# {{{ Function to switch packer/pacman-color, depending on options used
pac() {
  case $1 in

    -S | -Ss | -Ssq | -Si | -G )
      sudo packer $@ ;;

    -Su | -Syu )
      sudo packer $@
      echo "" > $HOME/.pacmanupdates ;;

    * )
      sudo pacman-color $@ ;;

  esac
} # }}}

# vim: set ft=zsh :
