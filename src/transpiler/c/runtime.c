#define _GNU_SOURCE
// #define _POSIX_C_SOURCE 200112L
#ifdef __EMSCRIPTEN__
#include <emscripten/html5.h>
#else
#include <execinfo.h>
#endif
#include <features.h>
#include <netdb.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __FILC__
#include <stdfil.h>
#endif

#define USE_GC
#ifdef USE_GC
#include <gc.h>
#endif

noreturn void exit_with_error(const char* s) {
#ifdef __FILC__
    zerror(s);
    exit(-1);
#else
    if (s != NULL) {
        fprintf(stderr, "%s\n", s);
    }
#ifndef __EMSCRIPTEN__
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
    exit(-1);
#endif
}

noreturn void Kast_match_non_exhaustive() {
    exit_with_error("Non exhausitve match");
}

noreturn void panic_errno() {
    perror("ERRNO");
    exit_with_error(NULL);
}

void* Kast_malloc(size_t size) {
#ifdef USE_GC
    void* result = GC_malloc(size);
#else
    void* result = malloc(size);
#endif
    if (!result) {
        panic_errno();
    }
    return result;
}

void* Kast_realloc(void* memory, size_t size) {
#ifdef USE_GC
    void* result = GC_realloc(memory, size);
#else
    void* result = realloc(memory, size);
#endif
    if (!result) {
        panic_errno();
    }
    return result;
}

void Kast_free(void* memory) {
#ifdef USE_GC
    GC_free(memory);
#else
    free(memory);
#endif
}

typedef struct {
} Unit;

typedef bool Bool;
typedef int32_t Int32;
typedef uint32_t UInt32;
typedef int64_t Int64;
typedef uint64_t UInt64;
typedef float Float32;
typedef double Float64;
typedef uint32_t Char;

typedef struct {
    uint64_t id;
} RawUnwindToken;

RawUnwindToken currently_unwinding = {.id = 0};

bool are_we_unwinding() {
    return currently_unwinding.id != 0;
}

bool are_we_unwinding_with(RawUnwindToken token) {
    return currently_unwinding.id == token.id;
}

void stop_unwinding() {
    currently_unwinding = (RawUnwindToken) {.id = 0};
}

uint64_t next_unwind_token_id = 1;

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

Char utf8_char_decode_step(const char** s) {
    size_t bytes = 0;
    while ((**s) & (1 << (7 - bytes))) {
        bytes++;
    }
    if (bytes == 0) {
        bytes = 1;
    }
    Char result = 0;
    for (size_t i = 0; i < bytes; i++) {
        char c = *((*s)++);
        int bits = i == 0 ? 7 : 6;
        c &= (1 << bits) - 1;
        result = (result << bits) | c;
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
} String;

String Char_to_String(Char c) {
    size_t len = Char_utf8_len(c);
    char* buf = Kast_malloc(len);
    char* encoder = buf;
    utf8_char_encode_step(&encoder, c);
    return (String) {.buf = buf, .length = len};
};

Char String_at(String s, size_t idx) {
    const char* decoder = s.buf + idx;
    return utf8_char_decode_step(&decoder);
}

size_t String_length(String s) {
    return s.length;
}

size_t String_utf8_length(String s) {
    return s.length;
}

int String_cmp(String a, String b) {
    for (size_t i = 0; i < a.length && i < b.length; i++) {
        int c = a.buf[i] - b.buf[i];
        if (c != 0) {
            return c;
        }
    }
    return a.length - b.length;
}

String String_concat(String a, String b) {
    char* buf = Kast_malloc(a.length + b.length);
    memcpy(buf, a.buf, a.length);
    memcpy(buf + a.length, b.buf, b.length);
    return (String) {
        .buf = buf,
        .length = a.length + b.length,
    };
}

typedef const char* C_String;

void Kast_write(FILE* f, String s) {
    fwrite(s.buf, sizeof(char), s.length, f);
}

String String_from_C_String(const C_String s) {
    return (String) {
        .buf = s,
        .length = strlen(s),
    };
}

char* String_to_C_String(const String s) {
    char* result = Kast_malloc(s.length + 1);
    memcpy(result, s.buf, s.length);
    result[s.length] = 0;
    return result;
}

noreturn void default_panic_handler(const String s) {
    fprintf(stderr, "Unhandled panic: ");
    Kast_write(stderr, s);
    fprintf(stderr, "\n");
    exit_with_error(NULL);
}

typedef struct {
    int argc;
    char** original_argv;
    String* argv;
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
#endif
}

void Kast_init(int argc, char* argv[]) {
#ifdef __EMSCRIPTEN__
    // Using solution 2 from boehmgc docs
    // https://github.com/bdwgc/bdwgc/blob/master/docs/platforms/README.emscripten
#ifdef USE_GC
    GC_disable();
    emscripten_set_interval(Kast_run_gc, 0, NULL);
#endif
#endif
    CLI_ARGS.argc = argc;
    CLI_ARGS.original_argv = argv;
    CLI_ARGS.argv = Kast_malloc(argc * sizeof(String));
    for (int i = 0; i < argc; i++) {
        CLI_ARGS.argv[i] = (String) {
            .buf = argv[i],
            .length = strlen(argv[i]),
        };
    }
}

void* Kast_ensure_correct_malloc(void* buf, size_t size) {
#ifdef USE_GC
    char* gc_buf = Kast_malloc(size);
    memcpy(gc_buf, buf, size);
    free(buf);
    return gc_buf;
#else
    return buf;
#endif
}

String Kast_asprintf(const char* fmt, ...) {
    char* buf;
    va_list va;
    va_start(va, fmt);
    int length = vasprintf(&buf, fmt, va);
    va_end(va);
    if (length < 0) {
        panic_errno();
    }
    buf = Kast_ensure_correct_malloc(buf, length);
    return (String) {
        .buf = buf,
        .length = length,
    };
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

Int32 Int32_from_String(String s) {
    int result = 0;
    for (size_t i = 0; i < s.length; i++) {
        result = result * 10 + s.buf[i] - '0';
    }
    return result;
}

Float64 Float64_from_String(String s) {
    char* cs = String_to_C_String(s);
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
    char* buf = Kast_malloc(size);
    size_t read = 0;
    while (read < size) {
        size_t new_read = fread(buf, 1, size - read, f);
        if (!new_read) {
            break;
        }
        read += new_read;
    }
    check_ferror(f);
    return (String) {
        .buf = buf,
        .length = size,
    };
}

String Kast_read_to_end(FILE* f) {
    int res = fseek(f, 0, SEEK_END);
    if (res < 0) {
        panic_errno();
    }
    long size = ftell(f);
    if (size < 0) {
        panic_errno();
    }
    res = fseek(f, 0, SEEK_SET);
    if (res < 0) {
        panic_errno();
    }
    return Kast_read_exactly(f, size);
}

String Kast_read_file(String path) {
    char* path_c = String_to_C_String(path);
    FILE* f = fopen(path_c, "r");
    Kast_free(path_c);
    if (!f) {
        panic_errno();
    }
    String result = Kast_read_to_end(f);
    if (fclose(f) != 0) {
        panic_errno();
    }
    return result;
}

String Kast_read_until(FILE* f, Char c) {
    char* buf = NULL;
    size_t buf_size = 0;
    ssize_t length = getdelim(&buf, &buf_size, c, f);
    if (length < 0) {
        panic_errno();
    }
    buf = Kast_ensure_correct_malloc(buf, length);
    return (String) {
        .buf = buf,
        .length = length,
    };
}

String Kast_input(String prompt) {
    Kast_write(stdout, prompt);
    return Kast_read_until(stdin, '\n');
}

bool Kast_isatty(FILE* f) {
    int desc = fileno(f);
    if (desc < 0) {
        panic_errno();
    }
    return isatty(desc);
}

typedef struct Context Context;

#define define_ArrayList(T)                                                    \
    typedef struct {                                                           \
        T* buf;                                                                \
        size_t capacity;                                                       \
        size_t length;                                                         \
    } ArrayList_##T;                                                           \
                                                                               \
    ArrayList_##T ArrayList_##T##_new() {                                      \
        return (ArrayList_##T) {                                               \
            .buf = NULL,                                                       \
            .capacity = 0,                                                     \
            .length = 0,                                                       \
        };                                                                     \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_reserve(ArrayList_##T* list, size_t len) {            \
        if (list->capacity < len) {                                            \
            list->capacity = (list->capacity == 0) ? 4 : (list->capacity * 2); \
            if (len > list->capacity) {                                        \
                list->capacity = len;                                          \
            }                                                                  \
            list->buf = Kast_realloc(list->buf, list->capacity * sizeof(T));   \
        }                                                                      \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_push_back(ArrayList_##T* list, T x) {                 \
        ArrayList_##T##_reserve(list, list->length + 1);                       \
        list->buf[list->length++] = x;                                         \
    }

define_ArrayList(Int32);

#define define_closure_type(name, Ret, ...)                                    \
    typedef struct {                                                           \
        void* captured;                                                        \
        Ret (*f)(Context*, void*, __VA_ARGS__);                                \
    } name;

define_closure_type(fn_Int32_Char_Unit, void, Int32, Char);
define_closure_type(fn_Char_Unit, void, Char);

#define call_closure(TODO_unwind, _f, ...)                                     \
    (_f).f(ctx, (_f).captured, __VA_ARGS__)

void String_iteri(Context* ctx, String s, fn_Int32_Char_Unit consumer) {
    const char* iter = s.buf;
    while (iter - s.buf < s.length) {
        Char c = utf8_char_decode_step(&iter);
        call_closure(return, consumer, iter - s.buf, c);
    }
}

void String_iteri_rev(Context* ctx, String s, fn_Int32_Char_Unit consumer) {
    const char* iter = s.buf + s.length;
    while (iter > s.buf) {
        Char c = utf8_char_decode_step_rev(&iter);
        call_closure(return, consumer, iter - s.buf, c);
    }
}

void String_iter(Context* ctx, String s, fn_Char_Unit consumer) {
    const char* iter = s.buf;
    while (iter - s.buf < s.length) {
        Char c = utf8_char_decode_step(&iter);
        call_closure(return, consumer, c);
    }
}

String String_substring(String s, Int32 start, Int32 len) {
    return (String) {
        .buf = s.buf + start,
        .length = len,
    };
}

void Kast_chdir(String path) {
    char* path_c = String_to_C_String(path);
    int res = chdir(path_c);
    if (!res) {
        panic_errno();
    }
    Kast_free(path_c);
}

Int32 Kast_exec(String cmd) {
    char* cmd_c = String_to_C_String(cmd);
    int res = system(cmd_c);
    if (res == -1) {
        panic_errno();
    }
    Kast_free(cmd_c);
    return WEXITSTATUS(res);
}

String Kast_getenv(String name) {
    char* name_c = String_to_C_String(name);
    char* buf = getenv(name_c);
    Kast_free(name_c);
    return (String) {
        .buf = buf,
        .length = buf ? strlen(buf) : 0,
    };
}

typedef struct {
    int sock_fd;
    FILE* stream;
} tcp_Stream;

typedef struct {
    int fd;
} tcp_Listener;

tcp_Stream tcp_Stream_from_fd(int fd) {
    FILE* stream = fdopen(fd, "r+");
    if (!stream) {
        panic_errno();
    }
    return (tcp_Stream) {
        .sock_fd = fd,
        .stream = stream,
    };
}

tcp_Stream tcp_Stream_connect(String addr) {
    char* colon_pos = memchr(addr.buf, ':', addr.length);
    if (!colon_pos) {
        default_panic_handler(String_from_C_String("Expected host:port"));
    }
    String host = {
        .buf = addr.buf,
        .length = colon_pos - addr.buf,
    };
    char* host_c = String_to_C_String(host);
    String port_s = {
        .buf = colon_pos + 1,
        .length = addr.buf + addr.length - colon_pos - 1,
    };
    // Int32 port = Int32_from_String(port_s);
    char* port_c = String_to_C_String(port_s);
    struct addrinfo *ai, *rp;
    int res = getaddrinfo(host_c, port_c, NULL, &ai);
    if (res) {
        if (res == EAI_SYSTEM) {
            panic_errno();
        } else {
            fprintf(stderr, "getaddrinfo failed with %d", res);
            exit(-1);
        }
    }
    Kast_free(host_c);
    Kast_free(port_c);
    for (rp = ai; rp != NULL; rp = rp->ai_next) {
        int sock_fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sock_fd == -1) {
            panic_errno();
        }
        int res = connect(sock_fd, rp->ai_addr, rp->ai_addrlen);
        if (res == 0) {
            freeaddrinfo(ai);
            return tcp_Stream_from_fd(sock_fd);
        };
        // ignore errno, try next addr
    }
    freeaddrinfo(ai);
    default_panic_handler(String_from_C_String("Failed to connect"));
}

void tcp_Stream_close(tcp_Stream s) {
    int res = fclose(s.stream);
    if (res != 0) {
        panic_errno();
    }
}

String tcp_Stream_read_line(tcp_Stream* s) {
    return Kast_read_until(s->stream, '\n');
}

void tcp_Stream_write(tcp_Stream* s, String* data) {
    Kast_write(s->stream, *data);
}

tcp_Listener tcp_Listener_bind(String addr) {
    char* colon_pos = memchr(addr.buf, ':', addr.length);
    if (!colon_pos) {
        default_panic_handler(String_from_C_String("Expected host:port"));
    }
    String host = {
        .buf = addr.buf,
        .length = colon_pos - addr.buf,
    };
    char* host_c = String_to_C_String(host);
    String port_s = {
        .buf = colon_pos + 1,
        .length = addr.buf + addr.length - colon_pos - 1,
    };
    // Int32 port = Int32_from_String(port_s);
    char* port_c = String_to_C_String(port_s);
    struct addrinfo *ai, *rp;
    int res = getaddrinfo(host_c, port_c, NULL, &ai);
    if (res) {
        if (res == EAI_SYSTEM) {
            panic_errno();
        } else {
            fprintf(stderr, "getaddrinfo failed with %d", res);
            exit(-1);
        }
    }
    Kast_free(host_c);
    Kast_free(port_c);
    for (rp = ai; rp != NULL; rp = rp->ai_next) {
        int fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        int so_reuseaddr = true;
        setsockopt(
            fd,
            SOL_SOCKET,
            SO_REUSEADDR,
            &so_reuseaddr,
            sizeof(so_reuseaddr)
        );
        if (fd == -1) {
            panic_errno();
        }
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
    default_panic_handler(String_from_C_String("Failed to bind"));
}

void tcp_Listener_listen(tcp_Listener* l, int max_pending) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-fd-leak"
    int res = listen(l->fd, max_pending);
    if (res == -1) {
        panic_errno();
    }
#pragma GCC diagnostic pop
}

typedef struct {
    tcp_Stream stream;
    String addr;
} tcp_Listener_accepted;

tcp_Listener_accepted tcp_Listener_accept(tcp_Listener* l, bool close_on_exec) {
    int flags = 0;
    if (close_on_exec) {
        flags |= SOCK_CLOEXEC;
    }
    struct sockaddr addr;
    socklen_t addr_len = sizeof(addr);
    int fd = accept4(l->fd, &addr, &addr_len, flags);
    if (fd == -1) {
        panic_errno();
    }
    size_t host_len = 100;
    char host[host_len];
    size_t port_len = 100;
    char port[port_len];
    int res = getnameinfo(&addr, addr_len, host, host_len, port, port_len, 0);
    if (res) {
        if (res == EAI_SYSTEM) {
            panic_errno();
        } else {
            fprintf(stderr, "getnameinfo errored with %d\n", res);
            exit(-1);
        }
    }
    host_len = strlen(host);
    port_len = strlen(port);
    char* addr_c = Kast_malloc(host_len + 1 + port_len);
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
}

void tcp_Listener_close(tcp_Listener l) {
    int res = close(l.fd);
    if (res == -1) {
        panic_errno();
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
