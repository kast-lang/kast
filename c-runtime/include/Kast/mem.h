#ifndef __KAST_MEM_H__

#include "./__common__.h"
#include "./type_info.h"

void* Kast_allocate_array(TypeInfo* T, size_t length);
void* Kast_allocate(TypeInfo* T);
void* Kast_reallocate_array(
    TypeInfo* T,
    void* a,
    size_t old_length,
    size_t new_length
);
void Kast_free(const void* memory);
void* Kast_allocate_raw(size_t length);

#endif
