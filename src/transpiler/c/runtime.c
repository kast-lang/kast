#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    char* buf = malloc(len);
    char* encoder = buf;
    utf8_char_encode_step(&encoder, c);
    return (String) {.buf = buf, .length = len};
};

typedef const char* C_String;

void print_String_to(FILE* f, String s) {
    fwrite(s.buf, sizeof(char), s.length, f);
}

void print_String(String s) {
    print_String_to(stdout, s);
}

String String_from_C_String(const C_String s) {
    return (String) {
        .buf = s,
        .length = strlen(s),
    };
}

void default_panic_handler(const String s) {
    fprintf(stderr, "Unhandled panic: ");
    print_String_to(stderr, s);
    exit(-1);
}

typedef struct {
    int argc;
    String* argv;
} CliArgs;

CliArgs CLI_ARGS;

void init_cli_args(int argc, char* argv[]) {
    CLI_ARGS.argc = argc;
    CLI_ARGS.argv = malloc(argc * sizeof(String));
    for (int i = 0; i < argc; i++) {
        CLI_ARGS.argv[i] = (String) {
            .buf = argv[i],
            .length = strlen(argv[i]),
        };
    }
}
