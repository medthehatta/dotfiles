# Check for an interactive session
[ -z "$PS1" ] && return

export PATH=$PATH:/home/med/scripts

alias ls='ls --color=auto --sort=time'
alias cls='clear; ls'
alias psu='ps -U $USER -o pid,cmd'
alias x='exit'
alias off='sudo shutdown -h now'

alias open='xdg-open'


export PS1='\!:$ '
export TZ='America/New_York'

export tussh='tud48344@astro.temple.edu'
export tuhome='/usr/home/c/141/tud48344'
export tuwork="${tuhome}/public_html/work"
alias tucw='ssh -t $tussh vim public_html/coursework.htm'
export enote='medthehatta.6e7a2@m.evernote.com'
export st0r='med@st0rage.org'
export dfrpglogs='med@st0rage.org:/home/med/public_html/dfrpg-logs'


