#ifndef __KAST_REF_COUNTED_H__
#define __KAST_REF_COUNTED_H__

#include "./__common__.h"
#include "./mem.h"

#define define_RefCounted(T)                                                   \
    typedef struct {                                                           \
        size_t ref_count;                                                      \
        T value;                                                               \
    } RefCounted_##T;                                                          \
    RefCounted_##T* RefCounted_##T##_new(T value);                             \
    RefCounted_##T* RefCounted_##T##_claim(RefCounted_##T* rc);                \
    size_t RefCounted_##T##_drop(RefCounted_##T* rc);

#define impl_RefCounted(T)                                                     \
    RefCounted_##T* RefCounted_##T##_new(T value) {                            \
        RefCounted_##T* rc;                                                    \
        rc = Kast_allocate_raw(sizeof(*rc));                                   \
        *rc = (RefCounted_##T) {                                               \
            .ref_count = 1,                                                    \
            .value = value,                                                    \
        };                                                                     \
        return rc;                                                             \
    }                                                                          \
    RefCounted_##T* RefCounted_##T##_claim(RefCounted_##T* rc) {               \
        rc->ref_count++;                                                       \
        return rc;                                                             \
    }                                                                          \
    size_t RefCounted_##T##_drop(RefCounted_##T* rc) {                         \
        rc->ref_count--;                                                       \
        size_t left_refs = rc->ref_count;                                      \
        if (left_refs == 0) {                                                  \
            Kast_free(rc);                                                     \
        }                                                                      \
        return left_refs;                                                      \
    }

define_RefCounted(Unit);

#endif
