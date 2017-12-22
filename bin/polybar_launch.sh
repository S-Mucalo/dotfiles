#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

for i in $(polybar -m | awk -F: '{print $1}');
do
    MONITOR=$i polybar top_bar -c ~/.config/polybar/config &
done

# polybar top -c ~/.config/polybar/config_2
# feh --bg-scale ~/.wallpaper/wall.png

feh --recursive --randomize --bg-fill ~/.wallpaper
echo "Bars launched..."
