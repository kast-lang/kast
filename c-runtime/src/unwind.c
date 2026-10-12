#include <Kast/unwind.h>

thread_local uint64_t next_unwind_token_id = 1;

thread_local RawUnwindToken currently_unwinding = {.id = 0};

bool are_we_unwinding(void) {
    return currently_unwinding.id != 0;
}

bool are_we_unwinding_with(RawUnwindToken token) {
    return currently_unwinding.id == token.id;
}

void start_unwinding(RawUnwindToken target) {
    currently_unwinding = target;
}

void stop_unwinding(void) {
    currently_unwinding = (RawUnwindToken) {.id = 0};
}

RawUnwindToken RawUnwindToken_new(void) {
    return (RawUnwindToken) {
        .id = next_unwind_token_id++,
    };
}
