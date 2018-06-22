# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# History tuning
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=16384
export HISTTIMEFORMAT="[%F %T] "
