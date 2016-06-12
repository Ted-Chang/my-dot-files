# Auto-screen invocation.
# if we're coming from a remote SSH connection, in an interactive session
# then automatically put us into a screen(1) session. Only try once
# -- if $STARTED_SCREEN is set, don't try it again, to avoid looping
# if screen fails for some reason.
if [ "$PS1" != "" -a "${SCREEN_STARTED}" != 1 -a "${SSH_TTY}" != "" ]; then
    SCREEN_STARTED=1; export SCREEN_STARTED
    sleep 1
    screen -RR && exit 0
    # normally, execution of this rc script ends here...
    echo "Start screen failed! Continue with normal bash startup"
fi
