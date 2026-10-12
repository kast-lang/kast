#include <Kast.h>

struct backtrace_state* BACKTRACE_STATE;

void Kast_backtrace_init(void) {
    BACKTRACE_STATE =
        backtrace_create_state(NULL, 1, Kast_Backtrace_error_callback, NULL);
}

void Kast_Backtrace_print(Kast_Backtrace* trace) {
    int frame_num = 0;
    for (Kast_Backtrace_C_Entry* entry = trace->c_entries; entry != NULL;
         entry = entry->next) {
        backtrace_pcinfo(
            BACKTRACE_STATE,
            entry->pc,
            Kast_Backtrace_print_entry,
            Kast_Backtrace_error_callback,
            &frame_num
        );
    }
}

int Kast_Backtrace_print_entry(
    void* data,
    uintptr_t pc,
    const char* filename,
    int lineno,
    const char* function
) {
    int* frame_num = data;
    fprintf(
        stderr,
        "%d. %s() at %s:%d\n",
        (*frame_num)++,
        function ? function : "??",
        filename ? filename : "??",
        lineno
    );
    return 0;
}

void Kast_Backtrace_drop_boxed(Kast_Backtrace* trace) {
    Kast_Backtrace_drop(*trace);
    free(trace);
}

void Kast_Backtrace_drop(Kast_Backtrace trace) {
    Kast_Backtrace_C_Entry* entry = trace.c_entries;
    while (entry != NULL) {
        Kast_Backtrace_C_Entry* next = entry->next;
        free(entry);
        entry = next;
    }
}

Kast_Backtrace* Kast_Backtrace_get_boxed(void) {
    Kast_Backtrace* trace = malloc(sizeof(Kast_Backtrace));
    if (trace == NULL) {
        fprintf(stderr, "OOM allocating backtrace\n");
        _exit(-1);
    }
    *trace = Kast_Backtrace_get();
    return trace;
}

Kast_Backtrace Kast_Backtrace_get(void) {
    Kast_Backtrace trace = {
        .c_entries = NULL,
    };
    backtrace_full(
        BACKTRACE_STATE,
        1,
        Kast_Backtrace_callback,
        Kast_Backtrace_error_callback,
        &trace
    );
    return trace;
}

int Kast_Backtrace_callback(
    void* data,
    uintptr_t pc,
    const char* filename,
    int lineno,
    const char* function
) {
    Kast_Backtrace* trace = data;
    Kast_Backtrace_C_Entry* new_entry = malloc(sizeof(Kast_Backtrace_C_Entry));
    if (new_entry == NULL) {
        fprintf(stderr, "OOM when getting backtrace");
        exit(-1);
    }
    *new_entry = (Kast_Backtrace_C_Entry) {
        .pc = pc,
        .next = trace->c_entries,
    };
    trace->c_entries = new_entry;
    return 0;
}

void Kast_Backtrace_error_callback(void* data, const char* msg, int errnum) {
    fprintf(stderr, "libbacktrace error: %s (errnum: %d)\n", msg, errnum);
    exit(-1);
}
