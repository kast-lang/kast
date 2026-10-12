#ifndef __KAST_CLAIM_H__
#define __KAST_CLAIM_H__

#include "./backtrace.h"

typedef enum {
    Kast_Claimed_State_Moved,
    Kast_Claimed_State_Owned
} Kast_Claimed_State;

typedef struct {
    const char* name;
    Kast_Claimed_State state;
#ifdef KAST_DEBUG
    Kast_Backtrace* trace;
#endif
} Kast_Claimed;

Kast_Claimed Kast_Claimed_init(const char* name);
void Kast_Claimed_access(Kast_Claimed* claimed);
void Kast_mark_as_claimed(Kast_Claimed* claimed);
Kast_Claimed_State Kast_Claimed_drop(Kast_Claimed claimed);

#endif
