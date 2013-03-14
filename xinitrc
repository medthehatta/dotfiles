# Default browser is chromium
export BROWSER=chromium

# Fonts
xset +fp /usr/share/fonts/local/
xset +fp /usr/share/fonts/artwiz-fonts/
xset +fp /usr/share/fonts/misc/

# I shouldn't have to do this...
#xrdb -merge ~/.Xdefaults

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
#HOME=/home/med/.dropbox1 dropbox start 
#HOME=/home/med/.dropbox2 dropbox start 
#drop stop
#drop start

# If the external monitor is connected, turn it on
#if [ $(xrandr -q | grep VGA1 | cut -d' ' -f2) == 'connected' ]; then
#	xrandr --output LVDS1 --mode 1024x600 --output VGA1 --auto --pos 1024x0 --right-of LVDS1
#fi
# Non-laptop version of above:
#xrandr --output DVI1 --mode 1280x1024 --output VGA1 --mode 1024x768 --right-of DVI1
# Something is seriously wrong.  Let's try this:
xrandr --output DVI1 --mode 1280x1024 --right-of VGA1
xrandr --output DVI1 --mode 1280x1024 --left-of VGA1

# xflux changes the color temperature of my screen based on the time of day
#xflux -z 19462 -k 4000

# Bashrun takes forever on first load, so start now
bashrun2 hide

# XMonad FTW
exec xmonad

