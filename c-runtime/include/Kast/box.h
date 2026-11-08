#ifndef __KAST_BOX_H__
#define __KAST_BOX_H__

#include "./__common__.h"
#include "./dbg.h"

#define define_Box(name, T)                                                    \
    typedef struct {                                                           \
        T* value;                                                              \
        TypeInfo* T_TypeInfo;                                                  \
        Kast_Claimed claimed;                                                  \
    } name;                                                                    \
    name name##_new(T value, TypeInfo* T_TypeInfo);                            \
    T* name##_deref(name* box);                                                \
    T* name##_leak(name box);                                                  \
    name name##_from_raw(T* ptr, TypeInfo* T_TypeInfo);                        \
    void name##_dbg_write(name* box, Kast_Formatter* fmt);                     \
    name name##_claim(name* place);                                            \
    void name##_drop(name box);

#define impl_Box(name, T)                                                      \
    name name##_new(T value, TypeInfo* T_TypeInfo) {                           \
        T* boxed_value = Kast_allocate(T_TypeInfo);                            \
        *boxed_value = value;                                                  \
        return name##_from_raw(boxed_value, T_TypeInfo);                       \
    }                                                                          \
    T* name##_deref(name* box) {                                               \
        Kast_Claimed_access(&box->claimed);                                    \
        return box->value;                                                     \
    }                                                                          \
    T* name##_leak(name box) {                                                 \
        Kast_Claimed_access(&box.claimed);                                     \
        return box.value;                                                      \
    }                                                                          \
    name name##_from_raw(T* ptr, TypeInfo* T_TypeInfo) {                       \
        return (name) {                                                        \
            .value = ptr,                                                      \
            .T_TypeInfo = T_TypeInfo,                                          \
            .claimed = Kast_Claimed_init("Box"),                               \
        };                                                                     \
    }                                                                          \
    void name##_dbg_write(name* box, Kast_Formatter* fmt) {                    \
        Kast_Formatter_printf(fmt, "box ");                                    \
        Kast_dbg_write(box->value, box->T_TypeInfo, fmt);                      \
    }                                                                          \
    name name##_claim(name* place) {                                           \
        name moved = *place;                                                   \
        Kast_mark_as_claimed(&place->claimed);                                 \
        place->value = NULL;                                                   \
        return moved;                                                          \
    }                                                                          \
    void name##_drop(name box) {                                               \
        if (Kast_Claimed_drop(box.claimed) == Kast_Claimed_State_Owned) {      \
            if (box.T_TypeInfo->drop != NULL) {                                \
                box.T_TypeInfo->drop(box.value);                               \
            }                                                                  \
            Kast_free(box.value);                                              \
        }                                                                      \
    }

#endif
