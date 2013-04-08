# Check for an interactive session
[ -z "$PS1" ] && return

export PATH=$PATH:/home/med/scripts

# Aliases for speed
alias ls='ls --color=auto --sort=time'
alias cls='clear && echo -n "@" && pwd && ls'
alias psu='ps -U $USER -o pid,cmd'
alias x='exit'
alias off='sudo shutdown -h now'
alias ..='cd ..'
alias ...='cd ../..'
alias r='rox' 
alias g='git'
alias stor='ssh -t med@ssh.st0rage.org screen -rd'

# Some locale stuff
export PS1='\n\!$ '
export TZ='America/New_York'
export EDITOR='vim'

# For dealing with commonly used remote hosts
export tussh='tud48344@astro.temple.edu'
export tuhome='/usr/home/c/141/tud48344'
export tuwork="${tuhome}/public_html/work"
alias tucw='ssh -t $tussh vim public_html/coursework.htm'
export enote='medthehatta.6e7a2@m.evernote.com'
export st0r='med@ssh.st0rage.org'
alias albert='ssh -t tud48344@astro.temple.edu ssh -t Albert@155.247.51.165 /bin/bash --rcfile /home/Med/.bashrc'

#fasd, for dealing with recent files
eval "$(fasd --init auto)"
alias v='f -e vim'
alias j='z' # z is the default "cd" action in fasd now
alias o='a -e mimeo'
alias l='d -e ls'
alias rj='z -e rox'

#git
alias g='git'

#dropbox aliases
alias drop1='HOME=/home/med/.dropbox1 dropbox'
alias drop2='HOME=/home/med/.dropbox2 dropbox'

