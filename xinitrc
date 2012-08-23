# Default browser is chromium
export BROWSER=chromium

# Fonts
xset +fp /usr/share/fonts/local/
xset +fp /usr/share/fonts/artwiz-fonts/
xset +fp /usr/share/fonts/misc/

# This lets me edit chromium textfields with vim
/usr/bin/perl ~/scripts/edit-server.pl &

# Restore sound mixer levels
alsactl restore

# Source my keymap
xmodmap ~/.xmodmap

# Start my status bar
/bin/bash ~/scripts/go_status.sh &

# Start my notification bar
#tail -f /tmp/msgs | dzen2 -l 5 -y 599 -fn cure -p &

# If the external monitor is connected, turn it on
if [ $(xrandr -q | grep VGA1 | cut -d' ' -f2) == 'connected' ]; then
	xrandr --output VGA1 --auto --right-of LVDS1
fi

# xflux changes the color temperature of my screen based on the time of day
xflux -z 19462 -k 4000

# XMonad FTW
exec xmonad

