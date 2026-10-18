#ifndef __KAST_TYPE_INFO_H__
#define __KAST_TYPE_INFO_H__

#include "./__common__.h"
#ifdef KAST_ALLOCATION_STATS
#include "./allocation_stats.h"
#endif
#include "./fmt.h"

struct TypeInfo {
    const char* name;
    size_t alignment;
    size_t size;
    size_t stride;
    void (*dbg_write)(void*, Kast_Formatter* fmt);
    void (*drop)(void*);
    void (*claim)(void* place, void* result);
#ifdef KAST_ALLOCATION_STATS
    Kast_type_allocation_stats allocation_stats;
#endif
};

#ifdef KAST_ALLOCATION_STATS
#define KAST_ALLOCATION_STATS_NEW_FIELD                                        \
    .allocation_stats = Kast_type_allocation_stats_new(),
#else
#define KAST_ALLOCATION_STATS_NEW_FIELD
#endif

#define TypeInfo_simple(kind, T)                                               \
    (TypeInfo) {                                                               \
        .name = #T, .alignment = alignof(T), .size = sizeof(T),                \
        .stride = sizeof(T), .dbg_write = T##_dbg_write_type_erased,           \
        KAST_ALLOCATION_STATS_NEW_FIELD                                        \
    }

void TypeInfo_drop(TypeInfo* T, void* value);

void Kast_init_type_infos(void);

void Byte_dbg_write(Byte* value, Kast_Formatter* fmt);
void Byte_dbg_write_type_erased(void* value, Kast_Formatter* fmt);

extern TypeInfo Byte_TypeInfo;

#endif
