#include <Kast/fmt.h>

Kast_Formatter Kast_Formatter_new(Kast_Writer writer) {
    return (Kast_Formatter) {
        .indent_printed = false,
        .indent = 0,
        .writer = writer,
    };
}

void Kast_Formatter_drop(Kast_Formatter fmt) {
    (void)fmt;
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
