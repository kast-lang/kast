#ifndef __KAST_IO_H__
#define __KAST_IO_H__

#include "./__common__.h"
#include "./array_list.h"
#include "./string.h"

extern Kast_Writer Kast_stdout_writer;
extern Kast_Writer Kast_stderr_writer;

int Kast_fgetc(FILE* f);
Char Kast_fgetChar(FILE* f);
void Kast_write(FILE* f, StringView s);
void Kast_write_c(FILE* f, C_StringView s);

String Kast_read_exactly(FILE* f, size_t size);
String Kast_read_to_end(FILE* f);
String Kast_read_file(StringView path);
ArrayList_Byte Kast_read_file_bytes(StringView path);
String Kast_read_until(FILE* f, Char delimiter);
String Kast_input(StringView prompt);
bool Kast_isatty(FILE* f);

#endif
