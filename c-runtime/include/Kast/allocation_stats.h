#ifndef __KAST_ALLOCATION_STATS_H__
#define __KAST_ALLOCATION_STATS_H__

#include "./__common__.h"

typedef struct {
    size_t allocations;
    size_t total_memory;
    bool tracked;
    struct TypeInfo* next_tracked;
} Kast_type_allocation_stats;

#define Kast_type_allocation_stats_new()                                       \
    ((Kast_type_allocation_stats) {                                            \
        .allocations = 0,                                                      \
        .total_memory = 0,                                                     \
        .tracked = false,                                                      \
    })

void Kast_ensure_type_is_tracked(TypeInfo* T);
void Kast_type_allocation_stats_dump(Kast_type_allocation_stats* stats);
void Kast_dump_allocation_stats(void);

#endif
