#include <Kast.h>
#include <Kast/string.h>

#ifdef USE_LIBBACKTRACE
struct backtrace_state* BACKTRACE_STATE;

int Kast_Backtrace_print_entry(
    void* data,
    uintptr_t pc,
    const char* filename,
    int lineno,
    const char* function
) {
    int* frame_num = data;
    fprintf(stderr, "%d. %s() at ", (*frame_num)++, function ? function : "??");
    if (filename != NULL) {
        fprintf(stderr, "%s:%d\n", filename ? filename : "??", lineno);
    } else {
        fprintf(stderr, "?? (%p)\n", (void*)pc);
    }
    return 0;
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
#endif

void Kast_backtrace_init(void) {
#ifdef USE_LIBBACKTRACE
    BACKTRACE_STATE =
        backtrace_create_state(NULL, 1, Kast_Backtrace_error_callback, NULL);
#endif
}

void Kast_Backtrace_print(Kast_Backtrace* trace) {
#ifdef USE_LIBBACKTRACE
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
#else
    backtrace_symbols_fd(trace->trace, trace->trace_len, fileno(stderr));
    fprintf(stderr, "That was C backtrace, now we try our best:\n");
    char** symbols = backtrace_symbols(trace->trace, trace->trace_len);
    for (size_t i = 0; i < trace->trace_len; i++) {
        char* symbol = symbols[i];
        size_t symbol_len = strlen(symbol);
        size_t plus_pos = 0;
        size_t addr_start = 0;
        size_t fn_start = 0;
        for (size_t i = symbol_len; i-- > 0;) {
            if (symbol[i] == '[') {
                addr_start = i + 1;
            }
            if (symbol[i] == '+') {
                plus_pos = i;
            }
            if (symbol[i] == '(' && plus_pos > i) {
                fn_start = i + 1;
            }
        }
        if (addr_start == 0) {
            continue;
        }
        fprintf(stderr, "%zu. ", i);
        if (fn_start != 0) {
            int fn_name_len = plus_pos - fn_start;
            const char* fn_name = symbol + fn_start;
            if (fn_name_len == 0) {
                fn_name = "??";
                fn_name_len = strlen(fn_name);
            }
            fprintf(stderr, "%.*s", fn_name_len, fn_name);
        } else {
            fprintf(stderr, "??");
        }
        fprintf(stderr, "() at ");
        String addr2line_command = Kast_asprintf(
            "addr2line -e %s %.*s",
            CLI_ARGS.original_argv[0],
            (int)(symbol_len - 1 - addr_start),
            symbol + addr_start
        );
        Kast_ExecResult result = Kast_exec_and_capture_output(
            String_as_StringView(&addr2line_command)
        );
        String_drop(addr2line_command);
        StringView sout = String_as_StringView(&result.sout);
        char* cwd_c = getcwd(NULL, 0);
        StringView cwd = StringView_from_C_StringView(cwd_c);
        if (sout.length >= cwd.length + 1
            && StringView_cmp(cwd, String_substring(sout, 0, cwd.length)) == 0
            && sout.buf[cwd.length] == '/') {
            Kast_write(
                stderr,
                String_substring(
                    sout,
                    cwd.length + 1,
                    sout.length - cwd.length - 1
                )
            );
        } else {
            Kast_write(stderr, sout);
        }
        free(cwd_c);
        Kast_ExecResult_drop(result);
    }
    free(symbols);
#endif
}

void Kast_Backtrace_drop_boxed(Kast_Backtrace* trace) {
    Kast_Backtrace_drop(*trace);
    free(trace);
}

void Kast_Backtrace_drop(Kast_Backtrace trace) {
#ifdef USE_LIBBACKTRACE
    Kast_Backtrace_C_Entry* entry = trace.c_entries;
    while (entry != NULL) {
        Kast_Backtrace_C_Entry* next = entry->next;
        free(entry);
        entry = next;
    }
#else
    (void)trace;
#endif
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
#ifdef USE_LIBBACKTRACE
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
#else
    Kast_Backtrace trace;
    trace.trace_len = backtrace(trace.trace, KAST_BACKTRACE_MAX);
#endif
    return trace;
}
