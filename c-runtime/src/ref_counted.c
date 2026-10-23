#include <Kast/ref_counted.h>

void RefCounted_ref_dump_func(void* key, void* value) {
    (void)value;
    Kast_Backtrace* trace = key;
    fprintf(stderr, "RefCounted ref:\n");
    Kast_Backtrace_print(trace);
}

impl_RefCounted(Unit)
