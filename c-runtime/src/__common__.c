#include <Kast/__common__.h>
#include <Kast/backtrace.h>

noreturn void exit_with_error(const char* fmt, ...) {
    if (fmt != NULL) {
        va_list va;
        va_start(va, fmt);
        vfprintf(stderr, fmt, va);
        fprintf(stderr, "\n");
        va_end(va);
    }
#ifndef __EMSCRIPTEN__
#ifdef USE_BACKTRACE
    Kast_Backtrace trace = Kast_Backtrace_get();
    Kast_Backtrace_print(&trace);
    Kast_Backtrace_drop(trace);
#else
    int N = 100;
    void* buf[N];
    int n = backtrace(buf, N);
    backtrace_symbols_fd(buf, n, fileno(stderr));
    // char** strings = backtrace_symbols(buf, n);
    // for (int i = 0; i < n; i++) {
    //     char* s = strings[i];
    //     fprintf(stderr, "%d. %s\n", i + 1, s);
    // }
#endif
#endif
    _exit(-1); // _exit to prevent sanitizers output
    exit(-1);
}

noreturn void exit_errno(const char* fmt, ...) {
    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
    fprintf(stderr, "%s\n", strerror(errno));
    exit_with_error(NULL);
}
