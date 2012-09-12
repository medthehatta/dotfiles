# Default browser is chromium
export BROWSER=chromium

# Fonts
xset +fp /usr/share/fonts/local/
xset +fp /usr/share/fonts/artwiz-fonts/
xset +fp /usr/share/fonts/misc/

# I shouldn't have to do this...
xrdb -merge ~/.Xdefaults

# GAWD I HATE THE BEEP
xset -b

# This lets me edit chromium textfields with vim
/usr/bin/perl ~/scripts/edit-server.pl &

# Restore sound mixer levels
alsactl restore

# Source my keymap
xmodmap ~/.xmodmap

# Start my status bar
/bin/bash ~/scripts/go_status.sh &

# Start my dropboxen
HOME=/home/med/.dropbox1 dropbox start &
HOME=/home/med/.dropbox2 dropbox start &

# If the external monitor is connected, turn it on
if [ $(xrandr -q | grep VGA1 | cut -d' ' -f2) == 'connected' ]; then
	xrandr --output LVDS1 --mode 1024x600 --output VGA1 --auto --pos 1024x0 --right-of LVDS1
fi

# xflux changes the color temperature of my screen based on the time of day
xflux -z 19462 -k 4000

# XMonad FTW
exec xmonad

