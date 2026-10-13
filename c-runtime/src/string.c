#include <Kast/string.h>
#include <Kast/type_info.h>

TypeInfo String_TypeInfo = {
    .name = "String",
    .alignment = alignof(String),
    .stride = sizeof(String),
    .size = sizeof(String),
    .drop = String_drop_type_erased,
    .claim = String_claim_type_erased,
    .dbg_write = String_dbg_write_type_erased,
};
TypeInfo StringView_TypeInfo = {
    .name = "StringView",
    .alignment = alignof(StringView),
    .stride = sizeof(StringView),
    .size = sizeof(StringView),
    .drop = NULL,
    .claim = NULL,
    .dbg_write = StringView_dbg_write_type_erased,
};

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

StringView StringView_claim(StringView* place) {
    return *place;
}

void StringView_claim_type_erased(void* place_void, void* result_void) {
    StringView* place = place_void;
    StringView* result = result_void;
    *result = StringView_claim(place);
}

void StringView_drop(StringView s) {
    (void)s;
}

void StringView_drop_type_erased(void* s) {
    StringView_drop(*(StringView*)s);
}

String Char_to_String(Char c) {
    size_t len = Char_utf8_len(c);
    char* buf = Kast_allocate_raw(len);
    char* encoder = buf;
    utf8_char_encode_step(&encoder, c);
    return String_from_raw_parts(buf, len);
}

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

void String_dbg_write(String* s, Kast_Formatter* fmt) {
    Kast_Formatter_printf(fmt, "\"%.*s\"", s->length, s->buf);
}

void String_dbg_write_type_erased(void* s, Kast_Formatter* fmt) {
    String_dbg_write(s, fmt);
}

void StringView_dbg_write(StringView* s, Kast_Formatter* fmt) {
    Kast_Formatter_printf(fmt, "\"%.*s\"", s->length, s->buf);
}

void StringView_dbg_write_type_erased(void* s, Kast_Formatter* fmt) {
    StringView_dbg_write(s, fmt);
}

StringView String_as_StringView(const String* s) {
    return (StringView) {
        .buf = s->buf,
        .length = s->length,
    };
}

int String_cmp(String* a, String* b) {
    return StringView_cmp(String_as_StringView(a), String_as_StringView(b));
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
    if (buf != NULL) {
        memcpy(buf, s, length);
    }
    return String_from_raw_parts(buf, length);
}

char* StringView_to_C_String(const StringView s) {
    char* result = Kast_allocate_raw(s.length + 1);
    memcpy(result, s.buf, s.length);
    result[s.length] = 0;
    return result;
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
        exit_errno("Kast_asprintf");
    }
    return String_from_raw_parts(buf, length);
}

String String_from_StringView(StringView s) {
    char* buf = Kast_allocate_raw(s.length);
    if (buf != NULL) {
        memcpy(buf, s.buf, s.length);
    }
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
        call_closure(break, consumer, i, c);
    }
    fn_Int32_Char_Unit_drop(consumer);
}

void String_iteri_rev(Context* ctx, StringView s, fn_Int32_Char_Unit consumer) {
    const char* iter = s.buf + s.length;
    while (iter > s.buf) {
        Char c = utf8_char_decode_step_rev(&iter);
        Int32 i = iter - s.buf;
        call_closure(break, consumer, i, c);
    }
    fn_Int32_Char_Unit_drop(consumer);
}

void String_iter(Context* ctx, StringView s, fn_Char_Unit consumer) {
    const char* iter = s.buf;
    while (iter < s.buf + s.length) {
        Char c = utf8_char_decode_step(&iter);
        call_closure(break, consumer, c);
    }
    fn_Char_Unit_drop(consumer);
}

StringView String_substring(StringView s, Int32 start, Int32 length) {
    return (StringView) {
        .buf = s.buf + start,
        .length = length,
    };
}
