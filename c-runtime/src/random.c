#include <Kast/random.h>

Int32 random_Int32(Int32 min, Int32 max) {
    return rand() % (max - min) + min;
}

Int64 random_Int64(Int64 min, Int64 max) {
    return ((((Int64)rand()) << 32) ^ (Int64)rand()) % (max - min) + min;
}

Float64 random_Float64(Float64 min, Float64 max) {
    return min + (max - min) * ((Float64)rand() / (Float64)RAND_MAX);
}
