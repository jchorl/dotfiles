if xrandr | grep -q "3840x2160"; then
    xrandr --output eDP-1 --off --output HDMI-2 --mode 3840x2160
else
    xrandr --output eDP-1 --mode 2560x1600
fi
