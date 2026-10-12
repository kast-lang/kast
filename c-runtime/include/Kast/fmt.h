#ifndef __KAST_FMT_H__
#define __KAST_FMT_H__

#include "./__common__.h"

typedef struct {
    void* user_data;
    void (*vprintf)(void*, const char* f, va_list);
} Kast_Writer;

typedef struct {
    bool indent_printed;
    int indent;
    Kast_Writer writer;
} Kast_Formatter;

Kast_Formatter Kast_Formatter_new(Kast_Writer writer);
void Kast_Formatter_drop(Kast_Formatter fmt);
void Kast_Formatter_printf(Kast_Formatter* fmt, const char* f, ...);
void Kast_Formatter_println(Kast_Formatter* fmt);
void Kast_Formatter_inc_indent(Kast_Formatter* fmt);
void Kast_Formatter_dec_indent(Kast_Formatter* fmt);

#endif
