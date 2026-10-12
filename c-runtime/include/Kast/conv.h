#ifndef __KAST_CONV_H__
#define __KAST_CONV_H__

#include "./__common__.h"
#include "./string.h"

String Float32_to_String(Float32 x);
String Float64_to_String(Float64 x);
String Int32_to_String(Int32 x);
String Int64_to_String(Int64 x);
Int32 Int32_from_String(StringView s);
Int64 Int64_from_String(StringView s);
Float64 Float64_from_String(StringView s);

UInt64 Float64_to_bits(Float64 self);

#endif
