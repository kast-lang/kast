#ifndef __KAST_UNWIND_H__
#define __KAST_UNWIND_H__

#include "./__common__.h"

typedef struct {
    /* we dont need data but we want unique address on stack */
    char _unused;
} RawUnwindToken;

typedef struct {
    RawUnwindToken* currently_unwinding;
} UnwindState;

UnwindState UnwindState_new(void);
RawUnwindToken RawUnwindToken_new(UnwindState* state);

bool are_we_unwinding(UnwindState* state);
bool are_we_unwinding_with(UnwindState* state, RawUnwindToken* token);
void start_unwinding(UnwindState* state, RawUnwindToken* target);
void stop_unwinding(UnwindState* state);

#endif
