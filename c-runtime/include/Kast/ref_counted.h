#ifndef __KAST_REF_COUNTED_H__
#define __KAST_REF_COUNTED_H__

#include "./__common__.h"
#include "./backtrace.h"
#include "./dbg.h"
#include "./mem.h"
#include "./ptr_map.h"

void RefCounted_ref_dump_func(void* key, void* value);

#define define_RefCounted(name, T)                                             \
    typedef struct {                                                           \
        struct name##RcData* data;                                             \
        /* Kast_Backtrace* this_trace; */                                      \
    } name;                                                                    \
    name name##_new(T value, TypeInfo* T_TypeInfo);                            \
    T* name##_deref(name* rc);                                                 \
    name name##_claim(name* rc);                                               \
    size_t name##_drop(name rc);                                               \
    void name##_dbg_write(name* rc, Kast_Formatter* fmt);                      \
    Bool name##_ptr_eq(name* a, name* b);                                      \
    void name##_debug_dump_refs(name* rc);

#define impl_RefCounted(name, T)                                               \
    typedef struct name##RcData {                                              \
        _Atomic size_t ref_count;                                              \
        /* PtrMap refs; */                                                     \
        TypeInfo* T_TypeInfo;                                                  \
        T value;                                                               \
    } name##RcData;                                                            \
    name name##_new(T value, TypeInfo* T_TypeInfo) {                           \
        name##RcData* data;                                                    \
        data = Kast_allocate_raw(sizeof(*data));                               \
        /* PtrMap refs = PtrMap_new(); */                                      \
        /* Kast_Backtrace* this_trace = Kast_Backtrace_get_boxed(); */         \
        /* PtrMap_add(&refs, this_trace, NULL); */                             \
        *data = (name##RcData) {                                               \
            .T_TypeInfo = T_TypeInfo,                                          \
            .ref_count = 1,                                                    \
            .value = value, /* .refs = refs, */                                \
        };                                                                     \
        return (name) {                                                        \
            .data = data, /* .this_trace = this_trace, */                      \
        };                                                                     \
    }                                                                          \
    T* name##_deref(name* rc) {                                                \
        return &rc->data->value;                                               \
    }                                                                          \
    name name##_claim(name* rc) {                                              \
        atomic_fetch_add(&rc->data->ref_count, 1);                             \
        /* Kast_Backtrace* this_trace = Kast_Backtrace_get_boxed(); */         \
        /* PtrMap_add(&rc->data->refs, this_trace, NULL); */                   \
        return (name) {                                                        \
            .data = rc->data, /* .this_trace = this_trace, */                  \
        };                                                                     \
    }                                                                          \
    size_t name##_drop(name rc) {                                              \
        size_t ref_count_before = atomic_fetch_sub(&rc.data->ref_count, 1);    \
        size_t left_refs = ref_count_before - 1;                               \
        /* PtrMap_remove(&rc.data->refs, rc.this_trace); */                    \
        if (left_refs == 0) {                                                  \
            if (rc.data->T_TypeInfo->drop != NULL) {                           \
                rc.data->T_TypeInfo->drop(&rc.data->value);                    \
            }                                                                  \
            /* PtrMap_drop(rc.data->refs); */                                  \
            Kast_free(rc.data);                                                \
        }                                                                      \
        /* Kast_Backtrace_drop_boxed(rc.this_trace); */                        \
        return left_refs;                                                      \
    }                                                                          \
    void name##_dbg_write(name* rc, Kast_Formatter* fmt) {                     \
        Kast_Formatter_printf(fmt, "rc ");                                     \
        Kast_dbg_write(&rc->data->value, rc->data->T_TypeInfo, fmt);           \
    }                                                                          \
    Bool name##_ptr_eq(name* a, name* b) {                                     \
        return a->data == b->data;                                             \
    }                                                                          \
    void name##_debug_dump_refs(name* rc) {                                    \
        fprintf(stderr, "debug dump refs is commented out\n");                 \
        /* PtrMap_iter(&rc->data->refs, RefCounted_ref_dump_func); */          \
    }

define_RefCounted(RefCounted_Unit, Unit);

#endif
