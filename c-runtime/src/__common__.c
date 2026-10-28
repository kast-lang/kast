#include <Kast/__common__.h>
#include <Kast/backtrace.h>

noreturn void exit_with_error(const char* fmt, ...) {
    if (fmt != NULL) {
        va_list va;
        va_start(va, fmt);
        vfprintf(stderr, fmt, va);
        va_end(va);
        fprintf(stderr, "\n");
    }
#ifndef __EMSCRIPTEN__
    Kast_Backtrace trace = Kast_Backtrace_get();
    Kast_Backtrace_print(&trace);
    Kast_Backtrace_drop(trace);
#endif
    _exit(-1); // _exit to prevent sanitizers output
    exit(-1);
}

noreturn void exit_errno(const char* fmt, ...) {
    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
    fprintf(stderr, ": %s\n", strerror(errno));
    exit_with_error(NULL);
}
