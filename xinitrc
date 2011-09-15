# Default browser is chromium
export BROWSER=chromium

# This lets me edit chromium textfields with vim
/usr/bin/perl ~/scripts/edit-server.pl &

# Restore sound mixer levels
alsactl restore

# Source my keymap
xmodmap ~/.xmodmap

# Start my status bar
/bin/bash ~/scripts/go_status.sh &

# If the external monitor is connected, turn it on
if [ $(xrandr -q | grep VGA1 | cut -d' ' -f2) == 'connected' ]; then
	xrandr --output VGA1 --mode 1280x1024 --above LVDS1
fi

# xflux changes the color temperature of my screen based on the time of day
xflux -z 19462 -k 4000

# XMonad FTW
exec xmonad
