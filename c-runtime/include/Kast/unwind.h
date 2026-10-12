#ifndef __KAST_UNWIND_H__
#define __KAST_UNWIND_H__

#include "./__common__.h"

typedef struct {
    uint64_t id;
} RawUnwindToken;

RawUnwindToken RawUnwindToken_new(void);

bool are_we_unwinding(void);
bool are_we_unwinding_with(RawUnwindToken token);
void start_unwinding(RawUnwindToken target);
void stop_unwinding(void);

#endif
