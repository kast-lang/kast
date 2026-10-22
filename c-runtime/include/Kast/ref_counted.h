#ifndef __KAST_REF_COUNTED_H__
#define __KAST_REF_COUNTED_H__

#include "./__common__.h"
#include "./dbg.h"
#include "./mem.h"

#define define_RefCounted(T)                                                   \
    typedef struct {                                                           \
        size_t ref_count;                                                      \
        TypeInfo* T_TypeInfo;                                                  \
        T value;                                                               \
    } RefCountedData_##T;                                                      \
    typedef RefCountedData_##T* RefCounted_##T;                                \
    RefCounted_##T RefCounted_##T##_new(T value, TypeInfo* T_TypeInfo);        \
    T* RefCounted_##T##_deref(RefCounted_##T* rc);                             \
    RefCounted_##T RefCounted_##T##_claim(RefCounted_##T* rc);                 \
    size_t RefCounted_##T##_drop(RefCounted_##T rc);                           \
    void RefCounted_##T##_dbg_write(RefCounted_##T* rc, Kast_Formatter* fmt);

#define impl_RefCounted(T)                                                     \
    RefCounted_##T RefCounted_##T##_new(T value, TypeInfo* T_TypeInfo) {       \
        RefCounted_##T rc;                                                     \
        rc = Kast_allocate_raw(sizeof(*rc));                                   \
        *rc = (RefCountedData_##T) {                                           \
            .T_TypeInfo = T_TypeInfo,                                          \
            .ref_count = 1,                                                    \
            .value = value,                                                    \
        };                                                                     \
        return rc;                                                             \
    }                                                                          \
    T* RefCounted_##T##_deref(RefCounted_##T* rc) {                            \
        return &(*rc)->value;                                                  \
    }                                                                          \
    RefCounted_##T RefCounted_##T##_claim(RefCounted_##T* place) {             \
        RefCounted_##T rc = *place;                                            \
        rc->ref_count++;                                                       \
        return rc;                                                             \
    }                                                                          \
    size_t RefCounted_##T##_drop(RefCounted_##T rc) {                          \
        rc->ref_count--;                                                       \
        size_t left_refs = rc->ref_count;                                      \
        if (left_refs == 0) {                                                  \
            if (rc->T_TypeInfo->drop != NULL) {                                \
                rc->T_TypeInfo->drop(&rc->value);                              \
            }                                                                  \
            Kast_free(rc);                                                     \
        }                                                                      \
        return left_refs;                                                      \
    }                                                                          \
    void RefCounted_##T##_dbg_write(RefCounted_##T* rc, Kast_Formatter* fmt) { \
        Kast_Formatter_printf(fmt, "rc ");                                     \
        Kast_dbg_write(&(*rc)->value, (*rc)->T_TypeInfo, fmt);                 \
    }

define_RefCounted(Unit);

#endif
