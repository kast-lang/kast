#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void panic_errno() {
    perror("ERRNO");
    exit(-1);
}

void* try_malloc(size_t size) {
    void* result = malloc(size);
    if (!result) {
        panic_errno();
    }
    return result;
}

typedef struct {
} Unit;

typedef bool Bool;
typedef int32_t Int32;
typedef int64_t Int64;
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

typedef struct String {
    const char* buf;
    size_t length;
} String;

String Char_to_String(Char c) {
    size_t len = Char_utf8_len(c);
    char* buf = try_malloc(len);
    char* encoder = buf;
    utf8_char_encode_step(&encoder, c);
    return (String) {.buf = buf, .length = len};
};

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
    char* result = try_malloc(s.length + 1);
    memcpy(result, s.buf, s.length);
    result[s.length] = 0;
    return result;
}

void default_panic_handler(const String s) {
    fprintf(stderr, "Unhandled panic: ");
    Kast_write(stderr, s);
    exit(-1);
}

typedef struct {
    int argc;
    String* argv;
} CliArgs;

CliArgs CLI_ARGS;

void init_cli_args(int argc, char* argv[]) {
    CLI_ARGS.argc = argc;
    CLI_ARGS.argv = try_malloc(argc * sizeof(String));
    for (int i = 0; i < argc; i++) {
        CLI_ARGS.argv[i] = (String) {
            .buf = argv[i],
            .length = strlen(argv[i]),
        };
    }
}

String Float64_to_String(Float64 x) {
    char* buf;
    int length = asprintf(&buf, "%f", x);
    if (length < 0) {
        panic_errno();
    }
    return (String) {
        .buf = buf,
        .length = length,
    };
}

String Int32_to_String(Int32 x) {
    char* buf;
    int length = asprintf(&buf, "%d", x);
    if (length < 0) {
        panic_errno();
    }
    return (String) {
        .buf = buf,
        .length = length,
    };
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
    free(cs);
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
    char* buf = try_malloc(size);
    size_t read = 0;
    while (read < size) {
        size_t new_read = fread(buf, 1, size - read, f);
        if (!new_read) {
            break;
        }
        read += new_read;
    }
    check_ferror(f);
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
    free(path_c);
    if (!f) {
        panic_errno();
    }
    return Kast_read_to_end(f);
}

String Kast_read_until(FILE* f, Char c) {
    char* buf = NULL;
    ssize_t length = getdelim(&buf, 0, c, stdin);
    if (length < 0) {
        panic_errno();
    }
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

#define define_fn_type(name, Ret, ...)                                         \
    typedef struct {                                                           \
        void* captured;                                                        \
        Ret (*f)(Context*, void*, __VA_ARGS__);                                \
    } name;

define_fn_type(fn_Char_Unit, Unit, Char);

void String_iter(Context* ctx, String s, fn_Char_Unit consumer) {
    const char* iter = s.buf;
    while (iter - s.buf < s.length) {
        Char c = utf8_char_decode_step(&iter);
        consumer.f(ctx, consumer.captured, c);
    }
}
