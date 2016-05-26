define traceall
    set pagination off
    set logging off
    set log file backtraceall
    set logging on
    thread apply all where
    set logging off
end

