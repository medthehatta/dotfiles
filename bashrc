# Check for an interactive session
[ -z "$PS1" ] && return

export PATH=$PATH:/home/med/scripts

alias ls='ls --color=auto'
alias psu='ps -U $USER -o pid,cmd'


export PS1='\!:$ '
export TZ='America/New_York'
export tussh='tud48344@astro.temple.edu'
export tuhome='/usr/home/c/141/tud48344'
export enote='medthehatta.6e7a2@m.evernote.com'
export st0r='med@st0rage.org'


