# prompt initialization

# If we are an interactive shell
if [ "$PS1" ]; then
    # Setup a red prompt for root
    NORMAL="\[\e[0m\]"
    RED="\[\e[1;31m\]"
    if [[ $EUID == 0 ]]; then
        PS1="$RED\h:\w\\$ $NORMAL"
    else
        PS1="\u@h:\w\\$ "
    fi

    unset NORMAL RED
fi
