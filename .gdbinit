set filename-display basename
set history filename ~/.gdb_history
set history save on
set history size 8192

define traceall
    set pagination off
    set logging off
    set log file backtraceall
    set logging on
    thread apply all where
    set logging off
end

