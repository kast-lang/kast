#ifndef __KAST_BACKTRACE_H__
#define __KAST_BACKTRACE_H__

#include "./__common__.h"

void Kast_backtrace_init(void);

void Kast_Backtrace_error_callback(void* data, const char* msg, int errnum);
int Kast_Backtrace_callback(
    void* data,
    uintptr_t pc,
    const char* filename,
    int lineno,
    const char* function
);

typedef struct Kast_Backtrace_C_Entry {
    uintptr_t pc;
    struct Kast_Backtrace_C_Entry* next;
} Kast_Backtrace_C_Entry;

typedef struct {
    Kast_Backtrace_C_Entry* c_entries;
} Kast_Backtrace;

Kast_Backtrace Kast_Backtrace_get(void);
Kast_Backtrace* Kast_Backtrace_get_boxed(void);
void Kast_Backtrace_drop(Kast_Backtrace trace);
void Kast_Backtrace_drop_boxed(Kast_Backtrace* trace);
int Kast_Backtrace_print_entry(
    void* data,
    uintptr_t pc,
    const char* filename,
    int lineno,
    const char* function
);
void Kast_Backtrace_print(Kast_Backtrace* trace);

#endif
