#ifndef __KAST_BOX_H__
#define __KAST_BOX_H__

#include "./__common__.h"

#define define_Box(T)                                                          \
    typedef struct {                                                           \
        T* value;                                                              \
        TypeInfo* T_TypeInfo;                                                  \
        Kast_Claimed claimed;                                                  \
    } Box_##T

#define impl_Box(T)                                                            \
    Box_##T Box_##T##_new(T value, TypeInfo* T_TypeInfo) {                     \
        T* boxed_value = Kast_allocate(T_TypeInfo);                            \
        *boxed_value = value;                                                  \
        return (Box_##T) {                                                     \
            .value = boxed_value,                                              \
            .T_TypeInfo = T_TypeInfo,                                          \
            .claimed = Kast_Claimed_init("Box"),                               \
        };                                                                     \
    }                                                                          \
    T* Box_##T##_deref(Box_##T* box) {                                         \
        Kast_Claimed_access(&box->claimed);                                    \
        return box->value;                                                     \
    }                                                                          \
    T* Box_##T##_leak(Box_##T box) {                                           \
        Kast_Claimed_access(&box.claimed);                                     \
        return box.value;                                                      \
    }                                                                          \
    void Box_##T##_dbg_write(Box_##T* box, Kast_Formatter* fmt) {              \
        Kast_Formatter_printf(fmt, "box ");                                    \
        Kast_dbg_write(box->value, box->T_TypeInfo, fmt);                      \
    }                                                                          \
    Box_##T Box_##T##_claim(Box_##T* place) {                                  \
        Box_##T moved = *place;                                                \
        Kast_mark_as_claimed(&place->claimed);                                 \
        place->value = NULL;                                                   \
        return moved;                                                          \
    }                                                                          \
    void Box_##T##_drop(Box_##T box) {                                         \
        if (Kast_Claimed_drop(box.claimed) == Kast_Claimed_State_Owned) {      \
            if (box.T_TypeInfo->drop != NULL) {                                \
                box.T_TypeInfo->drop(box.value);                               \
            }                                                                  \
            Kast_free(box.value);                                              \
        }                                                                      \
    }

#endif
