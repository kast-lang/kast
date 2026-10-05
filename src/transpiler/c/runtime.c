// #define USE_SANITIZERS
// #define KAST_ALLOCATION_STATS
// #define USE_GC
// #define GC_ON_INTERVAL_EMSCRIPTEN

#ifdef USE_SANITIZERS
#include <sanitizer/lsan_interface.h>
#endif

/* idk where this is documented,
 * but we want old winsock.h instead of winsock2.h */
#ifdef _WIN32
#define _WINSOCKAPI_
#include <windows.h>
#undef near
#undef far
#endif

#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
#define __POSIX__
#endif

// #define _GNU_SOURCE
// #define _POSIX_C_SOURCE 200112L
#ifdef __EMSCRIPTEN__
#include <emscripten/html5.h>
#define thread_local
#else
#define USE_BACKTRACE
#include <pthread.h>
#define thread_local __thread
// NOTE: never use <threads.h> its borked (with beohmgc specifically)
#define GC_THREADS
#endif
#ifdef USE_BACKTRACE
#include <backtrace.h>
#else
#ifndef __EMSCRIPTEN__
#include <execinfo.h>
#endif
#endif

#ifdef _WIN32
// TODO windows networking
#else
#include <netdb.h>
#include <sys/socket.h>
#endif

#include <errno.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#ifdef __FILC__
#include <stdfil.h>
#endif

#ifdef USE_GC
#include <gc/gc.h>
#include <gc/gc_typed.h>
#endif

#ifdef USE_BACKTRACE
struct backtrace_state* BACKTRACE_STATE;

typedef struct TypeInfo TypeInfo;

typedef struct {
    size_t allocations;
    size_t total_memory;
    bool tracked;
    TypeInfo* next_tracked;
} Kast_type_allocation_stats;

#define Kast_type_allocation_stats_new()                                       \
    ((Kast_type_allocation_stats) {                                            \
        .allocations = 0,                                                      \
        .total_memory = 0,                                                     \
        .tracked = false,                                                      \
    })

void Kast_type_allocation_stats_dump(Kast_type_allocation_stats* stats) {
    fprintf(
        stderr,
        "allocations=%zu total_memory=%zu",
        stats->allocations,
        stats->total_memory
    );
}

#ifdef USE_GC
typedef enum {
    TypeInfoKind_primitive,
    TypeInfoKind_raw,
    TypeInfoKind_object,
    TypeInfoKind_N,
} TypeInfoKind;
#endif

typedef struct {
    void* user_data;
    void (*vprintf)(void*, const char* f, va_list);
} Kast_Writer;

typedef struct {
    bool indent_printed;
    int indent;
    Kast_Writer writer;
} Kast_Formatter;

Kast_Formatter Kast_Formatter_new(Kast_Writer writer) {
    return (Kast_Formatter) {
        .indent_printed = false,
        .indent = 0,
        .writer = writer,
    };
}

void Kast_Formatter_printf(Kast_Formatter* fmt, const char* f, ...) {
    if (!fmt->indent_printed) {
        fmt->indent_printed = true;
        for (int i = 0; i < fmt->indent; i++) {
            Kast_Formatter_printf(fmt, "    ");
        }
    }
    va_list va;
    va_start(va, f);
    fmt->writer.vprintf(fmt->writer.user_data, f, va);
    va_end(va);
}

void Kast_Formatter_println(Kast_Formatter* fmt) {
    fmt->indent_printed = true;
    Kast_Formatter_printf(fmt, "\n");
    fmt->indent_printed = false;
}

void Kast_Formatter_inc_indent(Kast_Formatter* fmt) {
    fmt->indent++;
}

void Kast_Formatter_dec_indent(Kast_Formatter* fmt) {
    fmt->indent--;
}

struct TypeInfo {
    const char* name;
    size_t alignment;
    size_t size;
    size_t stride;
    void (*dbg_write)(void*, Kast_Formatter* fmt);
    void (*drop)(void*);
    void (*claim)(void* place, void* result);
#ifdef KAST_ALLOCATION_STATS
    Kast_type_allocation_stats allocation_stats;
#endif
#ifdef USE_GC
    TypeInfoKind kind;
    size_t gc_inner_ptrs;
    GC_descr gc_descr;
#endif
};

#ifdef KAST_ALLOCATION_STATS
#define KAST_ALLOCATION_STATS_NEW_FIELD                                        \
    .allocation_stats = Kast_type_allocation_stats_new(),
#else
#define KAST_ALLOCATION_STATS_NEW_FIELD
#endif

#ifdef USE_GC
#define KAST_GC_SIMPLE_TYPE_INFO_FIELDS(kind_value)                            \
    .kind = TypeInfoKind_##kind_value, .gc_inner_ptrs = 0,
#else
#define KAST_GC_SIMPLE_TYPE_INFO_FIELDS(kind)
#endif

#define TypeInfo_simple(kind, T)                                               \
    (TypeInfo) {                                                               \
        .name = #T, .alignment = alignof(T), .size = sizeof(T),                \
        .stride = sizeof(T), .dbg_write = T##_dbg_write_type_erased,           \
        KAST_ALLOCATION_STATS_NEW_FIELD KAST_GC_SIMPLE_TYPE_INFO_FIELDS(kind)  \
    }

typedef struct {
} Unit;

typedef bool Bool;
typedef char Byte;
typedef int32_t Int32;
typedef uint32_t UInt32;
typedef int64_t Int64;
typedef uint64_t UInt64;
typedef float Float32;
typedef double Float64;
typedef uint32_t Char;

void Kast_stderr_vprintf(void* user_data, const char* f, va_list va) {
    vfprintf(stderr, f, va);
}

Kast_Writer Kast_stderr_writer = {
    .user_data = NULL,
    .vprintf = Kast_stderr_vprintf,
};

void Kast_dbg_write(void* value, TypeInfo* T, Kast_Formatter* fmt) {
    if (T->dbg_write != NULL) {
        T->dbg_write(value, fmt);
    } else {
        Kast_Formatter_printf(fmt, "<unknown>");
    }
}

void Kast_dbg_print(void* value, TypeInfo* T) {
    Kast_Formatter fmt = Kast_Formatter_new(Kast_stderr_writer);
    Kast_dbg_write(value, T, &fmt);
    Kast_Formatter_println(&fmt);
}

void Byte_dbg_write(Byte value, Kast_Formatter* fmt) {
    Kast_Formatter_printf(fmt, "0x%02X", value);
}

void Byte_dbg_write_type_erased(void* value, Kast_Formatter* fmt) {
    Byte_dbg_write(*((Byte*)value), fmt);
}

TypeInfo Byte_TypeInfo = TypeInfo_simple(primitive, Byte);
TypeInfo String_TypeInfo;
TypeInfo StringView_TypeInfo;

void Kast_backtrace_error_callback(void* data, const char* msg, int errnum) {
    fprintf(stderr, "libbacktrace error: %s (errnum: %d)\n", msg, errnum);
    exit(-1);
}

typedef struct Kast_Backtrace_C_Entry {
    uintptr_t pc;
    const char* filename;
    int lineno;
    const char* function;
    struct Kast_Backtrace_C_Entry* next;
} Kast_Backtrace_C_Entry;

typedef struct {
    Kast_Backtrace_C_Entry* c_entries;
} Kast_Backtrace;

char* C_String_clone(const char* s) {
    if (s == NULL) {
        return NULL;
    }
    size_t length = strlen(s);
    char* cloned = malloc(length + 1);
    strcpy(cloned, s);
    return cloned;
}

int Kast_backtrace_callback(
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
        .filename = C_String_clone(filename),
        .lineno = lineno,
        .function = C_String_clone(function),
        .next = trace->c_entries,
    };
    trace->c_entries = new_entry;
    return 0;
}

Kast_Backtrace Kast_Backtrace_get() {
    Kast_Backtrace trace = {
        .c_entries = NULL,
    };
    backtrace_full(
        BACKTRACE_STATE,
        1,
        Kast_backtrace_callback,
        Kast_backtrace_error_callback,
        &trace
    );
    return trace;
}

Kast_Backtrace* Kast_Backtrace_get_boxed() {
    Kast_Backtrace* trace = malloc(sizeof(Kast_Backtrace));
    *trace = Kast_Backtrace_get();
    return trace;
}

void Kast_Backtrace_drop(Kast_Backtrace trace) {
    Kast_Backtrace_C_Entry* entry = trace.c_entries;
    while (entry != NULL) {
        Kast_Backtrace_C_Entry* next = entry->next;
        free((void*)entry->function);
        free((void*)entry->filename);
        free(entry);
        entry = next;
    }
}

void Kast_Backtrace_drop_boxed(Kast_Backtrace* trace) {
    Kast_Backtrace_drop(*trace);
    free(trace);
}

void Kast_Backtrace_print(Kast_Backtrace* trace) {
    int frame_num = 0;
    for (Kast_Backtrace_C_Entry* entry = trace->c_entries; entry != NULL;
         entry = entry->next) {
        fprintf(
            stderr,
            "%d. %s() at %s:%d\n",
            frame_num++,
            entry->function ? entry->function : "??",
            entry->filename ? entry->filename : "??",
            entry->lineno
        );
    }
}
#endif

noreturn void exit_with_error(const char* fmt, ...) {
#ifdef __FILC__
    zerror(s);
    exit(-1);
#else
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
#endif
}

typedef struct {
    Kast_Backtrace* trace;
    const char* name;
} Kast_Claimed;

Kast_Claimed Kast_Claimed_init(const char* name) {
    return (Kast_Claimed) {
        .trace = NULL,
        .name = name,
    };
}

void Kast_mark_as_claimed(Kast_Claimed* claimed) {
    if (claimed->trace != NULL) {
        fprintf(stderr, "%s was previously claimed here:\n", claimed->name);
        Kast_Backtrace_print(claimed->trace);
        exit_with_error("Trying to claim a moved %s", claimed->name);
    }
    claimed->trace = Kast_Backtrace_get_boxed();
}

typedef enum {
    Kast_Claimed_State_Moved,
    Kast_Claimed_State_Owned
} Kast_Claimed_State;

Kast_Claimed_State Kast_Claimed_drop(Kast_Claimed claimed) {
    if (claimed.trace != NULL) {
        Kast_Backtrace_drop_boxed(claimed.trace);
        return Kast_Claimed_State_Moved;
    } else {
        return Kast_Claimed_State_Owned;
    }
}

noreturn void Kast_match_non_exhaustive() {
    exit_with_error("Non exhausitve match");
}

noreturn void panic_errno(const char* s) {
    perror(s);
    exit_with_error(NULL);
}

#ifdef KAST_ALLOCATION_STATS
struct {
    Kast_type_allocation_stats by_kind[TypeInfoKind_N];
    Kast_type_allocation_stats raw;
} Kast_allocation_stats;

TypeInfo* TRACKED_TYPES_HEAD = NULL;

void Kast_ensure_type_is_tracked(TypeInfo* T) {
    if (T->allocation_stats.tracked) {
        return;
    }
    T->allocation_stats.tracked = true;
    T->allocation_stats.next_tracked = TRACKED_TYPES_HEAD;
    TRACKED_TYPES_HEAD = T;
}

int TypeInfo_compare(const void* void_a, const void* void_b) {
    TypeInfo* const* a = void_a;
    TypeInfo* const* b = void_b;
    return (int)(*a)->allocation_stats.allocations
        - (int)(*b)->allocation_stats.allocations;
}

void Kast_dump_allocation_stats() {
    GC_gcollect();
    fprintf(stderr, "RAW: ");
    Kast_type_allocation_stats_dump(&Kast_allocation_stats.raw);
    fprintf(stderr, "\n");
    for (size_t kind = 0; kind < TypeInfoKind_N; kind++) {
        fprintf(stderr, "kind=%zu: ", kind);
        Kast_type_allocation_stats_dump(&Kast_allocation_stats.by_kind[kind]);
        fprintf(stderr, "\n");
    }
    size_t types_len = 0;
    for (TypeInfo* T = TRACKED_TYPES_HEAD; T != NULL;
         T = T->allocation_stats.next_tracked) {
        types_len++;
    }
    TypeInfo** types = malloc(sizeof(TypeInfo*) * types_len);
    size_t i = 0;
    for (TypeInfo* T = TRACKED_TYPES_HEAD; T != NULL;
         T = T->allocation_stats.next_tracked) {
        types[i++] = T;
    }
    if (i != types_len) {
        exit_with_error("???");
    }
    qsort(types, types_len, sizeof(TypeInfo*), TypeInfo_compare);
    for (i = 0; i < types_len; i++) {
        TypeInfo* T = types[i];
        fprintf(stderr, "%s: ", T->name);
        Kast_type_allocation_stats_dump(&T->allocation_stats);
        fprintf(stderr, "\n");
    }
    free(types);
}

typedef struct {
    TypeInfo* T;
    size_t array_length;
} Kast_finalize_data;

void Kast_finalize(void* obj, void* void_data) {
    Kast_finalize_data* data = void_data;
    TypeInfo* T = data->T;
    if (T == &Byte_TypeInfo) {
        Kast_allocation_stats.raw.allocations--;
        Kast_allocation_stats.raw.total_memory -= data->array_length;
    }
    T->allocation_stats.allocations--;
    T->allocation_stats.total_memory -= T->stride * data->array_length;
    Kast_type_allocation_stats* kind_data =
        &Kast_allocation_stats.by_kind[T->kind];
    kind_data->allocations--;
    kind_data->total_memory -= T->stride * data->array_length;
    kind_data->scannable_ptrs -= T->gc_inner_ptrs;
    GC_FREE(data);
}

#else
void Kast_dump_allocation_stats() {
    fprintf(stderr, "KAST_ALLOCATION_STATS was not defined\n");
}
#endif

void* Kast_allocate_array(TypeInfo* T, size_t length) {
#ifdef USE_GC
    void* result;
    // result = GC_MALLOC(T->stride * length);
    switch (T->kind) {
        case TypeInfoKind_primitive:
            result = GC_MALLOC_ATOMIC(T->stride * length);
            break;
        case TypeInfoKind_raw:
            result = GC_MALLOC(T->stride * length);
            break;
        case TypeInfoKind_object:
#ifdef KAST_TYPED_GC
            result = GC_CALLOC_EXPLICITLY_TYPED(length, T->stride, T->gc_descr);
#else
            // TODO explicitly typed breaks with finalizers????
            result = GC_MALLOC(T->stride * length);
#endif
            break;
        case TypeInfoKind_N:
            exit_with_error("wrong type info kind");
    }
    if (!result) {
        panic_errno("Kast_allocate_array");
    }
#ifdef KAST_ALLOCATION_STATS
    Kast_ensure_type_is_tracked(T);
    Kast_finalize_data* finalize_data =
        GC_MALLOC_ATOMIC(sizeof(Kast_finalize_data));
    finalize_data->T = T;
    finalize_data->array_length = length;
    T->allocation_stats.allocations++;
    T->allocation_stats.total_memory += T->stride * length;
    if (T == &Byte_TypeInfo) {
        Kast_allocation_stats.raw.allocations++;
        Kast_allocation_stats.raw.total_memory += length;
    }
    Kast_type_allocation_stats* kind_data =
        &Kast_allocation_stats.by_kind[T->kind];
    kind_data->allocations++;
    kind_data->total_memory += T->stride * length;
    kind_data->scannable_ptrs += T->gc_inner_ptrs;
    GC_register_finalizer_no_order(
        result,
        Kast_finalize,
        finalize_data,
        NULL,
        NULL
    );
#endif
    return result;
#else
    void* result = malloc(T->stride * length);
    if (!result) {
        panic_errno("Kast_allocate_array");
    }
    return result;
#endif
}

void* Kast_allocate(TypeInfo* T) {
    return Kast_allocate_array(T, 1);
}

void* Kast_reallocate_array(
    TypeInfo* T,
    void* a,
    size_t old_length,
    size_t new_length
) {
#ifdef USE_GC
    void* result = Kast_allocate_array(T, new_length);
    if (a != NULL) {
        memcpy(result, a, T->stride * old_length);
    }
#else
    void* result = realloc(a, T->stride * new_length);
#endif
    if (!result) {
        panic_errno("Kast_reallocate_array");
    }
    return result;
}

void Kast_free(void* memory) {
#ifdef USE_GC
    GC_FREE(memory);
#else
    free(memory);
#endif
}

char* Kast_allocate_raw(size_t length) {
    return Kast_allocate_array(&Byte_TypeInfo, length);
}

void Kast_sleep_ns(int64_t ns) {
    time_t s = ns / 1000000000;
    ns %= 1000000000;
    struct timespec remaining = {.tv_sec = s, .tv_nsec = ns};
    while (true) {
        int res = nanosleep(&remaining, &remaining);
        if (res == 0) {
            break;
        }
        if (errno != EINTR) {
            panic_errno("Failed to sleep");
        }
    }
}

typedef struct {
    uint64_t id;
} RawUnwindToken;

thread_local RawUnwindToken currently_unwinding = {.id = 0};

bool are_we_unwinding() {
    return currently_unwinding.id != 0;
}

bool are_we_unwinding_with(RawUnwindToken token) {
    return currently_unwinding.id == token.id;
}

void start_unwinding(RawUnwindToken target) {
    currently_unwinding = target;
}

void stop_unwinding() {
    currently_unwinding = (RawUnwindToken) {.id = 0};
}

thread_local uint64_t next_unwind_token_id = 1;

RawUnwindToken RawUnwindToken_new() {
    return (RawUnwindToken) {
        .id = next_unwind_token_id++,
    };
}

size_t Char_utf8_len(Char c) {
    if (c <= 0x7f) {
        return 1;
    }
    if (c <= 0x7ff) {
        return 2;
    }
    if (c <= 0xffff) {
        return 3;
    }
    return 4;
}

size_t Char_utf16_len(Char c) {
    if (c <= 0xffff) {
        return 1;
    }
    return 2;
}

size_t Char_string_encoding_len(Char c) {
    return Char_utf8_len(c);
}

void utf8_char_encode_step(char** s, Char c) {
    size_t bytes = Char_utf8_len(c);
    for (size_t i = bytes - 1;; i--) {
        size_t bits = i == 0 ? 7 : 6;
        *(*s + i) = c & ((1 << bits) - 1);
        if (i == 0 && bytes > 1) {
            *(*s + i) |= ~((1 << (8 - bytes)) - 1);
        }
        if (i != 0) {
            *(*s + i) |= 0b10000000;
        }
        c >>= bits;
        if (i == 0) {
            break;
        }
    }
    *s += bytes;
}

void Char_dbg_write(Char* c, Kast_Formatter* fmt) {
    char s[4];
    char* out = s;
    utf8_char_encode_step(&out, *c);
    size_t byte_length = out - s;
    Kast_Formatter_printf(fmt, "'%.*s'", byte_length, s);
}

size_t Char_utf8_length_based_on_first_byte(char byte) {
    size_t length = 0;
    while (byte & (1 << (7 - length))) {
        length++;
    }
    if (length == 0) {
        length = 1;
    }
    return length;
}

int Kast_fgetc(FILE* f) {
    int res = fgetc(f);
    if (res == EOF) {
        if (feof(f)) {
            return EOF;
        }
        int err = ferror(f);
        if (err == 0) {
            exit_with_error("fgetc returned EOF, but not eof or error???");
        } else {
            exit_with_error(strerror(err));
        }
    }
    return res;
}

Char utf8_char_decode_step(const char** s) {
    size_t bytes = Char_utf8_length_based_on_first_byte(**s);
    Char result = 0;
    for (size_t i = 0; i < bytes; i++) {
        char c = *((*s)++);
        int bits = i == 0 ? 7 : 6;
        c &= (1 << bits) - 1;
        result = (result << bits) | c;
    }
    return result;
}

Char Kast_fgetChar(FILE* f) {
    int c = Kast_fgetc(f);
    if (c == EOF) {
        return EOF;
    }
    size_t bytes = Char_utf8_length_based_on_first_byte(c);
    char buf[4];
    buf[0] = c;
    size_t i = 1;
    while (i < bytes) {
        c = Kast_fgetc(f);
        if (c == EOF) {
            exit_with_error("EOF in the middle of utf-8");
        }
        buf[i++] = c;
    }
    const char* decoder = buf;
    Char result = utf8_char_decode_step(&decoder);
    size_t actually_decoded_bytes = decoder - buf;
    if (actually_decoded_bytes != bytes) {
        exit_with_error("utf8 decoder is wrong???");
    }
    return result;
}

Char utf8_char_decode_step_rev(const char** s) {
    for (;;) {
        (*s)--;
        if (((**s) & 0b11000000) != 0b10000000) {
            break;
        }
    };
    const char* decoder = *s;
    return utf8_char_decode_step(&decoder);
}

typedef struct String {
    const char* buf;
    size_t length;
    Kast_Claimed claimed;
} String;

String String_from_raw_parts(const char* buf, size_t length) {
    return (String) {
        .buf = buf,
        .length = length,
        .claimed = Kast_Claimed_init("String"),
    };
}

String String_claim(String* place) {
    String moved = *place;
    Kast_mark_as_claimed(&place->claimed);
    place->buf = NULL;
    place->length = 0;
    return moved;
}

void String_claim_type_erased(void* place_void, void* result_void) {
    String* place = place_void;
    String* result = result_void;
    *result = String_claim(place);
}

void String_drop(String s) {
    if (Kast_Claimed_drop(s.claimed) == Kast_Claimed_State_Owned) {
        Kast_free((void*)s.buf);
    }
}

void String_drop_type_erased(void* v) {
    String* s = v;
    String_drop(*s);
}

typedef struct {
    const char* buf;
    size_t length;
} StringView;

String Char_to_String(Char c) {
    size_t len = Char_utf8_len(c);
    char* buf = Kast_allocate_raw(len);
    char* encoder = buf;
    utf8_char_encode_step(&encoder, c);
    return String_from_raw_parts(buf, len);
};

Char String_at(StringView s, size_t idx) {
    const char* decoder = s.buf + idx;
    return utf8_char_decode_step(&decoder);
}

size_t String_length(StringView s) {
    return s.length;
}

size_t String_utf8_length(StringView s) {
    return s.length;
}

int StringView_cmp(StringView a, StringView b) {
    for (size_t i = 0; i < a.length && i < b.length; i++) {
        int c = (int)a.buf[i] - (int)b.buf[i];
        if (c != 0) {
            return c;
        }
    }
    return (int)a.length - (int)b.length;
}

typedef const char* C_String;
typedef const char* C_StringView;

void String_dbg_write(String* s, Kast_Formatter* fmt) {
    Kast_Formatter_printf(fmt, "\"%.*s\"", s->length, s->buf);
}

void StringView_dbg_write(StringView* s, Kast_Formatter* fmt) {
    Kast_Formatter_printf(fmt, "\"%.*s\"", s->length, s->buf);
}

void Kast_write(FILE* f, StringView s) {
    if (s.buf != NULL) {
        fwrite(s.buf, sizeof(char), s.length, f);
    }
}

StringView String_as_StringView(const String* s) {
    return (StringView) {
        .buf = s->buf,
        .length = s->length,
    };
}

StringView StringView_from_C_StringView(const C_StringView s) {
    return (StringView) {
        .buf = s,
        .length = strlen(s),
    };
}

String String_from_C_StringView(const C_StringView s) {
    size_t length = strlen(s);
    char* buf = Kast_allocate_raw(length);
    memcpy(buf, s, length);
    return String_from_raw_parts(buf, length);
}

char* StringView_to_C_String(const StringView s) {
    char* result = Kast_allocate_raw(s.length + 1);
    memcpy(result, s.buf, s.length);
    result[s.length] = 0;
    return result;
}

noreturn void default_panic_handler(const StringView s) {
    fprintf(stderr, "Unhandled panic: ");
    Kast_write(stderr, s);
    fprintf(stderr, "\n");
    exit_with_error(NULL);
}

typedef struct {
    int argc;
    char** original_argv;
    StringView* argv;
} CliArgs;

CliArgs CLI_ARGS;

#ifdef USE_GC
bool KAST_GC_ENABLED = true;

void Kast_run_gc(void* _data) {
    if (KAST_GC_ENABLED) {
        GC_enable();
        GC_gcollect();
        GC_disable();
    }
}
#endif

void Kast_init_user_type_infos();

void Kast_init_type_infos() {
    String_TypeInfo = (TypeInfo) {
        .name = "String",
        .alignment = alignof(String),
        .stride = sizeof(String),
        .size = sizeof(String),
        .drop = String_drop_type_erased,
        .claim = String_claim_type_erased,
#ifdef USE_GC
#ifdef KAST_ALLOCATION_STATS
        .allocation_stats = Kast_type_allocation_stats_new(),
#endif
        .kind = TypeInfoKind_object,
        .gc_inner_ptrs = 1,
        .gc_descr = ({
            GC_word T_bitmap[GC_BITMAP_SIZE(String)] = {0};
            GC_set_bit(T_bitmap, GC_WORD_OFFSET(String, buf));
            GC_descr descriptor =
                GC_make_descriptor(T_bitmap, GC_WORD_LEN(String));
            descriptor;
        }),
#endif
    };
    StringView_TypeInfo = (TypeInfo) {
        .name = "StringView",
        .alignment = alignof(StringView),
        .stride = sizeof(StringView),
        .size = sizeof(StringView),
#ifdef USE_GC
#ifdef KAST_ALLOCATION_STATS
        .allocation_stats = Kast_type_allocation_stats_new(),
#endif
        .kind = TypeInfoKind_object,
        .gc_inner_ptrs = 1,
        .gc_descr = ({
            GC_word T_bitmap[GC_BITMAP_SIZE(StringView)] = {0};
            GC_set_bit(T_bitmap, GC_WORD_OFFSET(StringView, buf));
            GC_descr descriptor =
                GC_make_descriptor(T_bitmap, GC_WORD_LEN(StringView));
            descriptor;
        }),
#endif
    };
}

void Kast_init(int argc, char* argv[]) {
    Kast_init_type_infos();
#ifdef USE_GC
    GC_INIT();
#endif
#ifdef __EMSCRIPTEN__
#if defined(USE_GC) && defined(GC_ON_INTERVAL_EMSCRIPTEN)
    // Using solution 2 from boehmgc docs
    // https://github.com/bdwgc/bdwgc/blob/master/docs/platforms/README.emscripten
    GC_disable();
    emscripten_set_interval(Kast_run_gc, 0, NULL);
#endif
#endif
#ifdef USE_BACKTRACE
    BACKTRACE_STATE =
        backtrace_create_state(NULL, 1, Kast_backtrace_error_callback, NULL);
#endif
    CLI_ARGS.argc = argc;
    CLI_ARGS.original_argv = argv;
    CLI_ARGS.argv = Kast_allocate_array(&StringView_TypeInfo, argc);
    for (int i = 0; i < argc; i++) {
        CLI_ARGS.argv[i] = (StringView) {
            .buf = argv[i],
            .length = strlen(argv[i]),
        };
    }
    Kast_init_user_type_infos();
}

String Kast_asprintf(const char* fmt, ...) {
    va_list va1, va2;
    va_start(va1, fmt);
    va_copy(va2, va1);
    int length = vsnprintf(NULL, 0, fmt, va1);
    va_end(va1);
    if (length < 0) {
        exit_with_error("determining asprintf length failed");
    }
    size_t buf_size = length + 1;
    char* buf = Kast_allocate_raw(buf_size);
    length = vsnprintf(buf, buf_size, fmt, va2);
    va_end(va2);
    if (length < 0) {
        panic_errno("Kast_asprintf");
    }
    return String_from_raw_parts(buf, length);
}

String Float32_to_String(Float32 x) {
    return Kast_asprintf("%f", x);
}

String Float64_to_String(Float64 x) {
    return Kast_asprintf("%f", x);
}

String Int32_to_String(Int32 x) {
    return Kast_asprintf("%d", x);
}

String Int64_to_String(Int64 x) {
    return Kast_asprintf("%ld", x);
}

Int32 Int32_from_String(StringView s) {
    // TODO negative, failures
    Int32 result = 0;
    for (size_t i = 0; i < s.length; i++) {
        result = result * 10 + s.buf[i] - '0';
    }
    return result;
}

Int64 Int64_from_String(StringView s) {
    // TODO negative, failures
    Int64 result = 0;
    for (size_t i = 0; i < s.length; i++) {
        result = result * 10 + s.buf[i] - '0';
    }
    return result;
}

Float64 Float64_from_String(StringView s) {
    char* cs = StringView_to_C_String(s);
    Float64 result = atof(cs);
    Kast_free(cs);
    return result;
}

void check_ferror(FILE* f) {
    int e = ferror(f);
    if (e) {
        fprintf(stderr, "File error %d (%s)\n", e, strerror(e));
        exit(-1);
    }
}

String Kast_read_exactly(FILE* f, size_t size) {
    char* buf = Kast_allocate_raw(size);
    size_t read = 0;
    while (read < size) {
        size_t new_read = fread(buf, 1, size - read, f);
        if (!new_read) {
            break;
        }
        read += new_read;
    }
    check_ferror(f);
    return String_from_raw_parts(buf, size);
}

String Kast_read_to_end(FILE* f) {
    int res = fseek(f, 0, SEEK_END);
    if (res < 0) {
        panic_errno("Kast_read_to_end.fseek(1)");
    }
    long size = ftell(f);
    if (size < 0) {
        panic_errno("Kast_read_to_end.ftell");
    }
    res = fseek(f, 0, SEEK_SET);
    if (res < 0) {
        panic_errno("Kast_read_to_end.fseek(2)");
    }
    return Kast_read_exactly(f, size);
}

String Kast_read_file(StringView path) {
    char* path_c = StringView_to_C_String(path);
    FILE* f = fopen(path_c, "r");
    Kast_free(path_c);
    if (!f) {
        panic_errno("Kast_read_file.fopen");
    }
    String result = Kast_read_to_end(f);
    if (fclose(f) != 0) {
        panic_errno("Kast_read_file.fclose");
    }
    return result;
}

String Kast_read_until(FILE* f, Char delimiter) {
    char* buf = NULL;
    size_t capacity = 0;
    size_t length = 0;
    for (;;) {
        Char c = Kast_fgetChar(f);
        if (c == EOF || c == delimiter) {
            break;
        }
        size_t encode_pos = length;
        length += Char_utf8_len(c);
        if (length > capacity) {
            size_t new_capacity = (capacity == 0) ? length : (capacity * 2);
            buf = Kast_reallocate_array(
                &Byte_TypeInfo,
                buf,
                capacity,
                new_capacity
            );
            capacity = new_capacity;
        }
        char* encoder = buf + encode_pos;
        utf8_char_encode_step(&encoder, c);
    }
    String result = {
        .buf = buf,
        .length = length,
    };
    return result;
}

String Kast_input(StringView prompt) {
    Kast_write(stdout, prompt);
    return Kast_read_until(stdin, '\n');
}

bool Kast_isatty(FILE* f) {
    int desc = fileno(f);
    if (desc < 0) {
        panic_errno("Kast_isatty");
    }
    return isatty(desc);
}

typedef struct Context Context;

#define define_ArrayList(T)                                                    \
    typedef struct {                                                           \
        TypeInfo* T_TypeInfo;                                                  \
        T* buf;                                                                \
        size_t capacity;                                                       \
        size_t length;                                                         \
        Kast_Claimed claimed;                                                  \
    } ArrayList_##T;

#define impl_ArrayList(T)                                                      \
    ArrayList_##T ArrayList_##T##_new(TypeInfo* T_TypeInfo) {                  \
        return (ArrayList_##T) {                                               \
            .T_TypeInfo = T_TypeInfo,                                          \
            .buf = NULL,                                                       \
            .capacity = 0,                                                     \
            .length = 0,                                                       \
            .claimed = Kast_Claimed_init("ArrayList"),                         \
        };                                                                     \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_dbg_write(ArrayList_##T* list, Kast_Formatter* fmt) { \
        Kast_Formatter_printf(fmt, "[");                                       \
        Kast_Formatter_inc_indent(fmt);                                        \
        Kast_Formatter_println(fmt);                                           \
        for (size_t i = 0; i < list->length; i++) {                            \
            Kast_dbg_write(&list->buf[i], list->T_TypeInfo, fmt);              \
            Kast_Formatter_printf(fmt, ",");                                   \
            Kast_Formatter_println(fmt);                                       \
        }                                                                      \
        Kast_Formatter_dec_indent(fmt);                                        \
        Kast_Formatter_printf(fmt, "]");                                       \
    }                                                                          \
                                                                               \
    ArrayList_##T ArrayList_##T##_claim(ArrayList_##T* list) {                 \
        ArrayList_##T moved = *list;                                           \
        Kast_mark_as_claimed(&list->claimed);                                  \
        return moved;                                                          \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_drop(ArrayList_##T list) {                            \
        if (Kast_Claimed_drop(list.claimed) == Kast_Claimed_State_Owned) {     \
            if (list.T_TypeInfo->drop != NULL) {                               \
                for (size_t i = 0; i < list.length; i++) {                     \
                    list.T_TypeInfo->drop(&list.buf[i]);                       \
                }                                                              \
            }                                                                  \
            Kast_free(list.buf);                                               \
        }                                                                      \
    }                                                                          \
                                                                               \
    ArrayList_##T ArrayList_##T##_with_capacity(                               \
        TypeInfo* T_TypeInfo,                                                  \
        size_t capacity                                                        \
    ) {                                                                        \
        return (ArrayList_##T) {                                               \
            .T_TypeInfo = T_TypeInfo,                                          \
            .buf = Kast_allocate_array(T_TypeInfo, capacity),                  \
            .capacity = capacity,                                              \
            .length = 0,                                                       \
        };                                                                     \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_reserve(ArrayList_##T* list, size_t len) {            \
        if (list->capacity < len) {                                            \
            size_t old_capacity = list->capacity;                              \
            list->capacity = (list->capacity == 0) ? 4 : (list->capacity * 2); \
            if (len > list->capacity) {                                        \
                list->capacity = len;                                          \
            }                                                                  \
            list->buf = Kast_reallocate_array(                                 \
                list->T_TypeInfo,                                              \
                list->buf,                                                     \
                old_capacity,                                                  \
                list->capacity                                                 \
            );                                                                 \
        }                                                                      \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_push_back(ArrayList_##T* list, T x) {                 \
        ArrayList_##T##_reserve(list, list->length + 1);                       \
        list->buf[list->length++] = x;                                         \
    }                                                                          \
                                                                               \
    T ArrayList_##T##_pop_back(ArrayList_##T* list) {                          \
        return list->buf[--list->length];                                      \
    }

#define define_closure_type(name, Ret, ...)                                    \
    typedef struct {                                                           \
        void* captured;                                                        \
        TypeInfo* captured_TypeInfo;                                           \
        Ret (*f)(Context*, void* __VA_OPT__(, ) __VA_ARGS__);                  \
    } name;                                                                    \
                                                                               \
    name name##_claim(name* place) {                                           \
        if (place->captured == NULL) {                                         \
            return *place;                                                     \
        }                                                                      \
        void* claimed_captured = Kast_allocate(place->captured_TypeInfo);      \
        place->captured_TypeInfo->claim(place->captured, claimed_captured);    \
        return (name) {                                                        \
            .captured = claimed_captured,                                      \
            .captured_TypeInfo = place->captured_TypeInfo,                     \
            .f = place->f,                                                     \
        };                                                                     \
    }                                                                          \
                                                                               \
    void name##_drop(name closure) {                                           \
        if (closure.captured == NULL) {                                        \
            return;                                                            \
        }                                                                      \
        if (closure.captured_TypeInfo->drop != NULL) {                         \
            closure.captured_TypeInfo->drop(closure.captured);                 \
        }                                                                      \
        Kast_free(closure.captured);                                           \
    }

define_closure_type(fn_Int32_Char_Unit, void, Int32, Char);
define_closure_type(fn_Char_Unit, void, Char);

#define call_closure(TODO_unwind, _f, ...)                                     \
    (_f).f(ctx, (_f).captured, __VA_ARGS__)

void TypeInfo_drop(TypeInfo* T, void* value) {
    if (T->drop != NULL) {
        T->drop(value);
    }
}

#define define_Box(T)                                                          \
    typedef struct {                                                           \
        T* value;                                                              \
        TypeInfo* T_TypeInfo;                                                  \
        Kast_Claimed claimed;                                                  \
    } Box_##T;

#define impl_Box(T)                                                            \
    Box_##T Box_##T##_new(T value, TypeInfo* T_TypeInfo) {                     \
        T* boxed_value = Kast_allocate(T_TypeInfo);                            \
        *boxed_value = value;                                                  \
        return (Box_##T) {                                                     \
            .value = boxed_value,                                              \
            .T_TypeInfo = T_TypeInfo,                                          \
            .claimed = Kast_Claimed_init("Box"),                               \
        };                                                                     \
    }                                                                          \
    void Box_##T##_dbg_write(Box_##T* box, Kast_Formatter* fmt) {              \
        Kast_Formatter_printf(fmt, "box ");                                    \
        Kast_dbg_write(box->value, box->T_TypeInfo, fmt);                      \
    }                                                                          \
    Box_##T Box_##T##_claim(Box_##T* place) {                                  \
        Box_##T moved = *place;                                                \
        Kast_mark_as_claimed(&place->claimed);                                 \
        place->value = NULL;                                                   \
        return moved;                                                          \
    }                                                                          \
    void Box_##T##_drop(Box_##T box) {                                         \
        if (Kast_Claimed_drop(box.claimed) == Kast_Claimed_State_Owned) {      \
            if (box.T_TypeInfo->drop != NULL) {                                \
                box.T_TypeInfo->drop(box.value);                               \
            }                                                                  \
            Kast_free(box.value);                                              \
        }                                                                      \
    }

String String_from_StringView(StringView s) {
    char* buf = Kast_allocate_raw(s.length);
    memcpy(buf, s.buf, s.length);
    return String_from_raw_parts(buf, s.length);
}

String String_concat(String a, String b) {
    if (a.length == 0) {
        String_drop(a);
        return b;
    }
    if (b.length == 0) {
        String_drop(b);
        return a;
    }
    char* buf = Kast_allocate_raw(a.length + b.length);
    memcpy(buf, a.buf, a.length);
    memcpy(buf + a.length, b.buf, b.length);
    size_t length = a.length + b.length;
    String_drop(a);
    String_drop(b);
    return String_from_raw_parts(buf, length);
}

void String_iteri(Context* ctx, StringView s, fn_Int32_Char_Unit consumer) {
    const char* iter = s.buf;
    while (iter < s.buf + s.length) {
        Int32 i = iter - s.buf;
        Char c = utf8_char_decode_step(&iter);
        call_closure(return, consumer, i, c);
    }
    fn_Int32_Char_Unit_drop(consumer);
}

void String_iteri_rev(Context* ctx, StringView s, fn_Int32_Char_Unit consumer) {
    const char* iter = s.buf + s.length;
    while (iter > s.buf) {
        Char c = utf8_char_decode_step_rev(&iter);
        Int32 i = iter - s.buf;
        call_closure(return, consumer, i, c);
    }
    fn_Int32_Char_Unit_drop(consumer);
}

void String_iter(Context* ctx, StringView s, fn_Char_Unit consumer) {
    const char* iter = s.buf;
    while (iter < s.buf + s.length) {
        Char c = utf8_char_decode_step(&iter);
        call_closure(return, consumer, c);
    }
    fn_Char_Unit_drop(consumer);
}

StringView String_substring(StringView s, Int32 start, Int32 length) {
    return (StringView) {
        .buf = s.buf + start,
        .length = length,
    };
}

void Kast_chdir(StringView path) {
    char* path_c = StringView_to_C_String(path);
    int res = chdir(path_c);
    if (res == -1) {
        panic_errno("Kast_chdir");
    }
    Kast_free(path_c);
}

Int32 Kast_exec(StringView cmd) {
    char* cmd_c = StringView_to_C_String(cmd);
    int res = system(cmd_c);
    Kast_free(cmd_c);
#ifdef __POSIX__
    if (res == -1) {
        panic_errno("Kast_exec");
    }
    return WEXITSTATUS(res);
#elif defined(_WIN32)
    if (res == -1) {
        panic_errno("Kast_exec");
    }
    return res;
#else
    UNKOWN_SYSTEM
#endif
}

String Kast_getenv(StringView name) {
    char* name_c = StringView_to_C_String(name);
    char* buf = getenv(name_c);
    Kast_free(name_c);
    return String_from_C_StringView(buf);
}

typedef struct {
    int sock_fd;
    FILE* reader;
    FILE* writer;
} tcp_Stream;

typedef struct {
    int fd;
} tcp_Listener;

tcp_Stream tcp_Stream_from_fd(int fd) {
    FILE* reader = fdopen(fd, "r");
    if (!reader) {
        panic_errno("tcp_Stream_from_fd");
    }
    FILE* writer = fdopen(fd, "w");
    if (!writer) {
        panic_errno("tcp_Stream_from_fd");
    }
    return (tcp_Stream) {
        .sock_fd = fd,
        .reader = reader,
        .writer = writer,
    };
}

tcp_Stream tcp_Stream_connect(StringView addr) {
#ifdef _WIN32
    exit_with_error("TODO tcp_Stream_connect windows");
#else
    char* colon_pos = memchr(addr.buf, ':', addr.length);
    if (!colon_pos) {
        exit_with_error("Expected host:port");
    }
    StringView host = {
        .buf = addr.buf,
        .length = colon_pos - addr.buf,
    };
    char* host_c = StringView_to_C_String(host);
    StringView port_s = {
        .buf = colon_pos + 1,
        .length = addr.buf + addr.length - colon_pos - 1,
    };
    // Int32 port = Int32_from_String(port_s);
    char* port_c = StringView_to_C_String(port_s);
    struct addrinfo *ai, *rp;
    int res = getaddrinfo(host_c, port_c, NULL, &ai);
    if (res) {
        if (res == EAI_SYSTEM) {
            panic_errno("tcp_Stream_connect.getaddrinfo");
        } else {
            fprintf(stderr, "getaddrinfo failed with %d", res);
            exit(-1);
        }
    }
    Kast_free(host_c);
    Kast_free(port_c);
    for (rp = ai; rp != NULL; rp = rp->ai_next) {
        if (rp->ai_socktype != SOCK_STREAM) {
            continue;
        }
        int sock_fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sock_fd == -1) {
            panic_errno("tcp_Stream_connect.socket");
        }
        int res = connect(sock_fd, rp->ai_addr, rp->ai_addrlen);
        if (res == 0) {
            freeaddrinfo(ai);
            return tcp_Stream_from_fd(sock_fd);
        };
        // ignore errno, try next addr
    }
    freeaddrinfo(ai);
    exit_with_error("Failed to connect");
#endif
}

void tcp_Stream_close(tcp_Stream s) {
    int res = fclose(s.reader);
    if (res != 0) {
        panic_errno("tcp_Stream_close.reader");
    }
    // Dont need to close writer since reader closes underlying fd
    // res = fclose(s.writer);
    // if (res != 0) {
    //     panic_errno("tcp_Stream_close.writer");
    // }
}

String tcp_Stream_read_line(tcp_Stream* s) {
    return Kast_read_until(s->reader, '\n');
}

void tcp_Stream_write(tcp_Stream* s, StringView data) {
    Kast_write(s->writer, data);
    if (fflush(s->writer) != 0) {
        panic_errno("tcp_Stream_write.fflush");
    }
}

tcp_Listener tcp_Listener_bind(StringView addr) {
#ifdef _WIN32
    exit_with_error("TODO tcp_Listener_bind windows");
#else
    char* colon_pos = memchr(addr.buf, ':', addr.length);
    if (!colon_pos) {
        exit_with_error("Expected host:port");
    }
    StringView host = {
        .buf = addr.buf,
        .length = colon_pos - addr.buf,
    };
    char* host_c = StringView_to_C_String(host);
    StringView port_s = {
        .buf = colon_pos + 1,
        .length = addr.buf + addr.length - colon_pos - 1,
    };
    // Int32 port = Int32_from_String(port_s);
    char* port_c = StringView_to_C_String(port_s);
    struct addrinfo *ai, *rp;
    int res = getaddrinfo(host_c, port_c, NULL, &ai);
    if (res) {
        if (res == EAI_SYSTEM) {
            panic_errno("tcp_Listener_bind.getaddrinfo");
        } else {
            fprintf(stderr, "getaddrinfo failed with %d", res);
            exit(-1);
        }
    }
    Kast_free(host_c);
    Kast_free(port_c);
    for (rp = ai; rp != NULL; rp = rp->ai_next) {
        int fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (fd == -1) {
            panic_errno("tcp_Listener_bind.socket");
        }
        int so_reuseaddr = true;
        setsockopt(
            fd,
            SOL_SOCKET,
            SO_REUSEADDR,
            &so_reuseaddr,
            sizeof(so_reuseaddr)
        );
        int res = bind(fd, rp->ai_addr, rp->ai_addrlen);
        if (res == 0) {
            freeaddrinfo(ai);
            return (tcp_Listener) {
                .fd = fd,
            };
        };
        // ignore errno, try next addr
    }
    freeaddrinfo(ai);
    exit_with_error("Failed to bind");
#endif
}

void tcp_Listener_listen(tcp_Listener* l, int max_pending) {
#ifdef _WIN32
    exit_with_error("TODO tcp_Listener_listen windows");
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-fd-leak"
    int res = listen(l->fd, max_pending);
    if (res == -1) {
        panic_errno("tcp_Listener_listen");
    }
#pragma GCC diagnostic pop
#endif
}

typedef struct {
    tcp_Stream stream;
    String addr;
} tcp_Listener_accepted;

tcp_Listener_accepted tcp_Listener_accept(tcp_Listener* l, bool close_on_exec) {
#ifdef _WIN32
    exit_with_error("TODO tcp_Listener_accept windows");
#else
    int flags = 0;
    if (close_on_exec) {
        flags |= SOCK_CLOEXEC;
    }
    struct sockaddr addr;
    socklen_t addr_len = sizeof(addr);
    // int fd = accept4(l->fd, &addr, &addr_len, flags);
    int fd = accept(l->fd, &addr, &addr_len);
    if (fd == -1) {
        panic_errno("tcp_Listener_accept.accept4");
    }
    size_t host_len = 100;
    char host[host_len];
    size_t port_len = 100;
    char port[port_len];
    int res = getnameinfo(&addr, addr_len, host, host_len, port, port_len, 0);
    if (res) {
        if (res == EAI_SYSTEM) {
            panic_errno("tcp_Listener_accept.getnameinfo");
        } else {
            fprintf(stderr, "getnameinfo errored with %d\n", res);
            exit(-1);
        }
    }
    host_len = strlen(host);
    port_len = strlen(port);
    char* addr_c = Kast_allocate_raw(host_len + 1 + port_len);
    memcpy(addr_c, host, host_len);
    addr_c[host_len] = ':';
    memcpy(addr_c + host_len + 1, port, port_len);
    String addr_s = {
        .buf = addr_c,
        .length = host_len + 1 + port_len,
    };
    return (tcp_Listener_accepted) {
        .stream = tcp_Stream_from_fd(fd),
        .addr = addr_s,
    };
#endif
}

void tcp_Listener_close(tcp_Listener l) {
    int res = close(l.fd);
    if (res == -1) {
        panic_errno("tcp_Listener_close");
    }
}

Int32 random_Int32(Int32 min, Int32 max) {
    return rand() % (max - min) + min;
}

Int64 random_Int64(Int64 min, Int64 max) {
    return ((((Int64)rand()) << 32) ^ (Int64)rand()) % (max - min) + min;
}

Float64 random_Float64(Float64 min, Float64 max) {
    return min + (max - min) * ((Float64)rand() / (Float64)RAND_MAX);
}

UInt64 Float64_to_bits(Float64 self) {
    UInt64 uint;
    memcpy(&uint, &self, sizeof(uint));
    return uint;
}
