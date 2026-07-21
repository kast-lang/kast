#include <stddef.h>
#include <stdio.h>
#include <string.h>

typedef struct String {
    const char* buf;
    size_t length;
} String;

typedef const char* C_String;

void print_String(String s) {
    fwrite(s.buf, sizeof(char), s.length, stdout);
}

String String_from_C_String(const C_String s) {
    return (String) {
        .buf = s,
        .length = strlen(s),
    };
}
