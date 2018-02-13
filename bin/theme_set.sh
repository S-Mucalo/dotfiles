#!/usr/bin/env sh

new_theme() {
    wal -i $(find $HOME/.wallpaper \( -iname "*.png" -or -iname "*.jpg" \) | shuf -n1)

    feh --bg-fill "$(< "${HOME}/.cache/wal/wal")"

    # feh --recursive --randomize --bg-fill ~/.wallpaper
    # feh --recursive --randomize --bg-scale ~/.wallpaper
    reload_dunst
}

reload_theme() {
    wal -R
    reload_dunst
}

reload_dunst() {
    # Source generated colors.
    . "${HOME}/.cache/wal/colors.sh"

    pkill dunst
    dunst \
        -lb "${color0:-#FFFFFF}" \
        -nb "${color0:-#FFFFFF}" \
        -cb "${color0:-#FFFFFF}" \
        -lf "${color15:-#000000}" \
        -bf "${color15:-#000000}" \
        -cf "${color15:-#000000}" \
        -nf "${color15:-#000000}" \
        -fn "Roboto 12" \
        -geometry "600x40-80+100" &
}

set_theme() {

    wal -i $1
    . "${HOME}/.cache/wal/colors.sh"
    reload_dunst
}

usage() {
    echo "usage: theme_set.sh [-n | --new] | [-R | --reload] | [-s | --set_theme] | [-h | --help]"
}

case $1 in
    -s | --set )           set_theme $2
                           ;;
    -n | --new )           new_theme
                           ;;
    -R | --reload )        reload_theme
                           ;;
    -h | --help )          usage
                           exit
                           ;;
    * )                    usage
                           exit 1
esac
