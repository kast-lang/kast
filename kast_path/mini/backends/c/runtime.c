// #include <gc.h>
//
// #define malloc GC_MALLOC

typedef struct RawUnwindToken {
  int id;
} RawUnwindToken;

const RawUnwindToken NO_UNWIND = {.id = 0};

RawUnwindToken current_unwinding_raw_token = NO_UNWIND;

int next_raw_unwind_token_id = 1;
