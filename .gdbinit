# Author: Ted Zhang <ted.g.zhang@live.com>

# Set prompt color
set prompt \033[31m(gdb) \033[0m

set confirm off
set verbose off

set filename-display basename
set history filename ~/.gdb_history
set history save on
set history size 8192

# Disable pagination
set pagination off

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
