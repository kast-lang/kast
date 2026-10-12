#include <Kast/conv.h>

String Float32_to_String(Float32 x) {
    return Kast_asprintf("%f", x);
}

String Float64_to_String(Float64 x) {
    return Kast_asprintf("%f", x);
}

String Int32_to_String(Int32 x) {
    return Kast_asprintf("%d", x);
}

String Int64_to_String(Int64 x) {
    return Kast_asprintf("%ld", x);
}

Int32 Int32_from_String(StringView s) {
    // TODO negative, failures
    Int32 result = 0;
    for (size_t i = 0; i < s.length; i++) {
        result = result * 10 + s.buf[i] - '0';
    }
    return result;
}

Int64 Int64_from_String(StringView s) {
    // TODO negative, failures
    Int64 result = 0;
    for (size_t i = 0; i < s.length; i++) {
        result = result * 10 + s.buf[i] - '0';
    }
    return result;
}

Float64 Float64_from_String(StringView s) {
    char* cs = StringView_to_C_String(s);
    Float64 result = atof(cs);
    Kast_free(cs);
    return result;
}

UInt64 Float64_to_bits(Float64 self) {
    UInt64 uint;
    memcpy(&uint, &self, sizeof(uint));
    return uint;
}
