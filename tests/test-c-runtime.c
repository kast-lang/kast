#include "../src/transpiler/c/runtime.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    char* buf = malloc(100500);
    size_t buf_len = 10;
    size_t len = getline(&buf, &buf_len, stdin);

    String s = {.buf = buf, .length = strlen(buf)};
    if (s.buf[s.length - 1] == '\n') {
        s.length -= 1;
    }
    const char* it = s.buf;
    size_t i = 0;

    char* outbuf = malloc(100500);
    char* out = outbuf;
    while ((it - s.buf) < s.length) {
        Char c = utf8_char_decode_step(&it);
        char printed[10];
        char* pp = printed;
        utf8_char_encode_step(&pp, c);
        printf("%zu = ", i);
        Kast_write(stdout, (String) {.buf = printed, .length = pp - printed});
        printf("\n");
        i++;
        utf8_char_encode_step(&out, c);
    }

    Kast_write(stdout, (String) {.buf = outbuf, .length = out - outbuf});
    printf("\n");

    free(buf);
    return 0;
}
