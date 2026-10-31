#include <Kast/unwind.h>

UnwindState UnwindState_new(void) {
    return (UnwindState) {
        .currently_unwinding = NULL,
    };
}

RawUnwindToken RawUnwindToken_new(UnwindState* state) {
    (void)state;
    return (RawUnwindToken) {0};
}

bool are_we_unwinding(UnwindState* state) {
    return state->currently_unwinding != NULL;
}

bool are_we_unwinding_with(UnwindState* state, RawUnwindToken* token) {
    return state->currently_unwinding == token;
}

void start_unwinding(UnwindState* state, RawUnwindToken* target) {
    state->currently_unwinding = target;
}

void stop_unwinding(UnwindState* state) {
    state->currently_unwinding = NULL;
}
