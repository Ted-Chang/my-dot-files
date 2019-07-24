# Author: Ted Zhang <ted.g.zhang@live.com>

# Set prompt color
set prompt \033[31m(gdb) \033[0m

set confirm off
set verbose off

set editing-mode emacs
set filename-display basename
set history filename ~/.gdb_history
set history save on
set history size 8192

set print pretty on

# Enable auto load .gdbinit in project path
set auto-load safe-path /

# Disable pagination
set pagination off

# Trace all threads
define traceall
    set logging off
    set log file ~/gdb_traceall.txt
    set logging redirect on
    set logging on
    thread apply all where
    set logging off
end
document traceall
Syntax: traceall
| Back trace all threads in the target
| Log appends to the file ~/gdb_traceall.txt
end

# Auto backtrace
define autobt
    commands
        silent
        # Add commands here to print info or do other things
        bt
        cont
    end
end
document autobt
Syntax: autobt <bp-num>
| Back trace and continue execution automatically 
| when the specified breakpoint was hit
end
