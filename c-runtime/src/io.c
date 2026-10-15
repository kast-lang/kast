#include <Kast/io.h>
#include <Kast/mem.h>

void Kast_write_c(FILE* f, const char* s) {
    if (fprintf(f, "%s", s) < 0) {
        exit_errno("Failed to write");
    }
}

void Kast_write(FILE* f, StringView s) {
    if (s.buf != NULL) {
        if (fwrite(s.buf, sizeof(char), s.length, f) != s.length) {
            exit_errno("Failed to write");
        }
    }
}

void Kast_stdout_vprintf(void* user_data, const char* f, va_list va) {
    (void)user_data;
    if (vfprintf(stdout, f, va) < 0) {
        exit_errno("Failed to write to stdout");
    }
}

Kast_Writer Kast_stdout_writer = {
    .user_data = NULL,
    .vprintf = Kast_stdout_vprintf,
};

void Kast_stderr_vprintf(void* user_data, const char* f, va_list va) {
    (void)user_data;
    if (vfprintf(stderr, f, va) < 0) {
        exit_errno("Failed to write to stdout");
    }
}

Kast_Writer Kast_stderr_writer = {
    .user_data = NULL,
    .vprintf = Kast_stderr_vprintf,
};

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
        exit_errno("Kast_read_to_end.fseek(1)");
    }
    long size = ftell(f);
    if (size < 0) {
        exit_errno("Kast_read_to_end.ftell");
    }
    res = fseek(f, 0, SEEK_SET);
    if (res < 0) {
        exit_errno("Kast_read_to_end.fseek(2)");
    }
    return Kast_read_exactly(f, size);
}

String Kast_read_file(StringView path) {
    char* path_c = StringView_to_C_String(path);
    FILE* f = fopen(path_c, "r");
    if (!f) {
        exit_errno("Kast_read_file.fopen(%s)", path_c);
    }
    Kast_free(path_c);
    String result = Kast_read_to_end(f);
    if (fclose(f) != 0) {
        exit_errno("Kast_read_file.fclose");
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
        exit_errno("Kast_isatty");
    }
    return isatty(desc);
}
