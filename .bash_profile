# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

# History tuning
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=16384
export HISTTIMEFORMAT="[%F %T] "
