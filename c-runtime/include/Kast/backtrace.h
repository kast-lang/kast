#ifndef __KAST_BACKTRACE_H__
#define __KAST_BACKTRACE_H__

#include "./__common__.h"

void Kast_backtrace_init(void);

#ifdef USE_LIBBACKTRACE
typedef struct Kast_Backtrace_C_Entry {
    uintptr_t pc;
    struct Kast_Backtrace_C_Entry* next;
} Kast_Backtrace_C_Entry;

typedef struct {
    Kast_Backtrace_C_Entry* c_entries;
} Kast_Backtrace;
#else
#define KAST_BACKTRACE_MAX 64

typedef struct {
    void* trace[KAST_BACKTRACE_MAX];
    size_t trace_len;
} Kast_Backtrace;
#endif

Kast_Backtrace Kast_Backtrace_get(void);
Kast_Backtrace* Kast_Backtrace_get_boxed(void);
void Kast_Backtrace_drop(Kast_Backtrace trace);
void Kast_Backtrace_drop_boxed(Kast_Backtrace* trace);
void Kast_Backtrace_print(Kast_Backtrace* trace);

#endif
