#include <Kast/dbg.h>
#include <Kast/io.h>

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

void Byte_dbg_write(Byte* value, Kast_Formatter* fmt) {
    Kast_Formatter_printf(fmt, "0x%02X", *value);
}

void Byte_dbg_write_type_erased(void* value, Kast_Formatter* fmt) {
    Byte_dbg_write(((Byte*)value), fmt);
}
