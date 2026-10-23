#ifndef __KAST_REF_COUNTED_H__
#define __KAST_REF_COUNTED_H__

#include "./__common__.h"
#include "./backtrace.h"
#include "./dbg.h"
#include "./mem.h"
#include "./ptr_map.h"

void RefCounted_ref_dump_func(void* key, void* value);

#define define_RefCounted(T)                                                   \
    typedef struct {                                                           \
        struct RefCountedData_##T* data;                                       \
        Kast_Backtrace* this_trace;                                            \
    } RefCounted_##T;                                                          \
    RefCounted_##T RefCounted_##T##_new(T value, TypeInfo* T_TypeInfo);        \
    T* RefCounted_##T##_deref(RefCounted_##T* rc);                             \
    RefCounted_##T RefCounted_##T##_claim(RefCounted_##T* rc);                 \
    size_t RefCounted_##T##_drop(RefCounted_##T rc);                           \
    void RefCounted_##T##_dbg_write(RefCounted_##T* rc, Kast_Formatter* fmt);  \
    void RefCounted_##T##_debug_dump_refs(RefCounted_##T* rc);

#define impl_RefCounted(T)                                                     \
    typedef struct RefCountedData_##T {                                        \
        size_t ref_count;                                                      \
        PtrMap refs;                                                           \
        TypeInfo* T_TypeInfo;                                                  \
        T value;                                                               \
    } RefCountedData_##T;                                                      \
    RefCounted_##T RefCounted_##T##_new(T value, TypeInfo* T_TypeInfo) {       \
        RefCountedData_##T* data;                                              \
        data = Kast_allocate_raw(sizeof(*data));                               \
        PtrMap refs = PtrMap_new();                                            \
        Kast_Backtrace* this_trace = Kast_Backtrace_get_boxed();               \
        PtrMap_add(&refs, this_trace, NULL);                                   \
        *data = (RefCountedData_##T) {                                         \
            .T_TypeInfo = T_TypeInfo,                                          \
            .ref_count = 1,                                                    \
            .value = value,                                                    \
            .refs = refs,                                                      \
        };                                                                     \
        return (RefCounted_##T) {                                              \
            .data = data,                                                      \
            .this_trace = this_trace,                                          \
        };                                                                     \
    }                                                                          \
    T* RefCounted_##T##_deref(RefCounted_##T* rc) {                            \
        return &rc->data->value;                                               \
    }                                                                          \
    RefCounted_##T RefCounted_##T##_claim(RefCounted_##T* rc) {                \
        rc->data->ref_count++;                                                 \
        Kast_Backtrace* this_trace = Kast_Backtrace_get_boxed();               \
        PtrMap_add(&rc->data->refs, this_trace, NULL);                         \
        return (RefCounted_##T) {                                              \
            .data = rc->data,                                                  \
            .this_trace = this_trace,                                          \
        };                                                                     \
    }                                                                          \
    size_t RefCounted_##T##_drop(RefCounted_##T rc) {                          \
        rc.data->ref_count--;                                                  \
        size_t left_refs = rc.data->ref_count;                                 \
        PtrMap_remove(&rc.data->refs, rc.this_trace);                          \
        if (left_refs == 0) {                                                  \
            if (rc.data->T_TypeInfo->drop != NULL) {                           \
                rc.data->T_TypeInfo->drop(&rc.data->value);                    \
            }                                                                  \
            PtrMap_drop(rc.data->refs);                                        \
            Kast_free(rc.data);                                                \
        }                                                                      \
        Kast_Backtrace_drop_boxed(rc.this_trace);                              \
        return left_refs;                                                      \
    }                                                                          \
    void RefCounted_##T##_dbg_write(RefCounted_##T* rc, Kast_Formatter* fmt) { \
        Kast_Formatter_printf(fmt, "rc ");                                     \
        Kast_dbg_write(&rc->data->value, rc->data->T_TypeInfo, fmt);           \
    }                                                                          \
    void RefCounted_##T##_debug_dump_refs(RefCounted_##T* rc) {                \
        PtrMap_iter(&rc->data->refs, RefCounted_ref_dump_func);                \
    }

define_RefCounted(Unit);

#endif
