#ifndef __KAST_SORT_H__
#define __KAST_SORT_H__

#include "./__common__.h"

void Kast_sort(
    void* a,
    size_t n,
    size_t size,
    int (*compare)(void* a, void* b, void* user),
    void* user
);

#endif
