#ifndef __KAST_STRING_H__
#define __KAST_STRING_H__

#include "./__common__.h"
#include "./claim.h"
#include "./closure.h"
#include "./fmt.h"

typedef struct String {
    const char* buf;
    size_t length;
    Kast_Claimed claimed;
} String;

typedef struct {
    const char* buf;
    size_t length;
} StringView;

typedef const char* C_String;
typedef const char* C_StringView;

extern TypeInfo String_TypeInfo;
extern TypeInfo StringView_TypeInfo;

size_t Char_utf8_len(Char c);
size_t Char_utf16_len(Char c);
size_t Char_string_encoding_len(Char c);
void utf8_char_encode_step(char** s, Char c);
Char utf8_char_decode_step(const char** s);
Char utf8_char_decode_step_rev(const char** s);
void Char_dbg_write(Char* c, Kast_Formatter* fmt);
size_t Char_utf8_length_based_on_first_byte(char byte);
String Char_to_String(Char c);

String String_from_raw_parts(const char* buf, size_t length);
String String_claim(String* place);
void String_claim_type_erased(void* place_void, void* result_void);
void String_drop(String s);
void String_drop_type_erased(void* v);
void String_dbg_write(String* s, Kast_Formatter* fmt);
void String_dbg_write_type_erased(void* s, Kast_Formatter* fmt);
StringView StringView_claim(StringView* place);
void StringView_claim_type_erased(void* place_void, void* result_void);
void StringView_drop(StringView s);
void StringView_drop_type_erased(void* v);
void StringView_dbg_write(StringView* s, Kast_Formatter* fmt);
void StringView_dbg_write_type_erased(void* s, Kast_Formatter* fmt);

StringView String_as_StringView(const String* s);
int String_cmp(String* a, String* b);
StringView StringView_from_C_StringView(const C_StringView s);
String String_from_C_StringView(const C_StringView s);
char* StringView_to_C_String(const StringView s);

Char String_at(StringView s, size_t idx);

size_t String_length(StringView s);

size_t String_utf8_length(StringView s);

int StringView_cmp(StringView a, StringView b);

String String_from_StringView(StringView s);
String String_concat(String a, String b);
void String_iteri(
    UnwindState* unwind,
    Context* ctx,
    StringView s,
    fn_Int32_Char_Unit consumer
);
void String_iteri_rev(
    UnwindState* unwind,
    Context* ctx,
    StringView s,
    fn_Int32_Char_Unit consumer
);
void String_iter(
    UnwindState* unwind,
    Context* ctx,
    StringView s,
    fn_Char_Unit consumer
);
StringView String_substring(StringView s, Int32 start, Int32 length);

String Kast_asprintf(const char* fmt, ...);

#endif
