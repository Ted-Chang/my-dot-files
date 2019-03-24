# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# If we are an interactive shell
if [ "$PS1" ]; then

    # Append to history file, don't overwrite it
    shopt -s histappend

    # History tuning
    export HISTCONTROL=ignoreboth:erasedups
    export HISTSIZE=16384
    export HISTTIMEFORMAT="[%F %T] "

    # Prompt initialization
    # Setup a red prompt for root
    NORMAL="\[\e[0m\]"
    RED="\[\e[1;31m\]"
    if [[ $EUID == 0 ]]; then
        PS1="$RED\h:\w\\$ $NORMAL"
    else
        PS1="\u@\h:\w\\$ "
    fi

    unset NORMAL RED
fi

