#ifndef __KAST_ARRAY_LIST_H__
#define __KAST_ARRAY_LIST_H__

#include "./__common__.h"
#include "./claim.h"

#define define_ArrayList(T)                                                    \
    typedef struct {                                                           \
        TypeInfo* T_TypeInfo;                                                  \
        T* buf;                                                                \
        size_t capacity;                                                       \
        size_t length;                                                         \
        Kast_Claimed claimed;                                                  \
    } ArrayList_##T

#define impl_ArrayList(T)                                                      \
    ArrayList_##T ArrayList_##T##_new(TypeInfo* T_TypeInfo) {                  \
        return (ArrayList_##T) {                                               \
            .T_TypeInfo = T_TypeInfo,                                          \
            .buf = NULL,                                                       \
            .capacity = 0,                                                     \
            .length = 0,                                                       \
            .claimed = Kast_Claimed_init("ArrayList"),                         \
        };                                                                     \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_dbg_write(ArrayList_##T* list, Kast_Formatter* fmt) { \
        Kast_Formatter_printf(fmt, "[");                                       \
        Kast_Formatter_inc_indent(fmt);                                        \
        Kast_Formatter_println(fmt);                                           \
        for (size_t i = 0; i < list->length; i++) {                            \
            Kast_dbg_write(&list->buf[i], list->T_TypeInfo, fmt);              \
            Kast_Formatter_printf(fmt, ",");                                   \
            Kast_Formatter_println(fmt);                                       \
        }                                                                      \
        Kast_Formatter_dec_indent(fmt);                                        \
        Kast_Formatter_printf(fmt, "]");                                       \
    }                                                                          \
                                                                               \
    ArrayList_##T ArrayList_##T##_claim(ArrayList_##T* list) {                 \
        ArrayList_##T moved = *list;                                           \
        Kast_mark_as_claimed(&list->claimed);                                  \
        return moved;                                                          \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_drop(ArrayList_##T list) {                            \
        if (Kast_Claimed_drop(list.claimed) == Kast_Claimed_State_Owned) {     \
            if (list.T_TypeInfo->drop != NULL) {                               \
                for (size_t i = 0; i < list.length; i++) {                     \
                    list.T_TypeInfo->drop(&list.buf[i]);                       \
                }                                                              \
            }                                                                  \
            Kast_free(list.buf);                                               \
        }                                                                      \
    }                                                                          \
                                                                               \
    ArrayList_##T ArrayList_##T##_with_capacity(                               \
        TypeInfo* T_TypeInfo,                                                  \
        size_t capacity                                                        \
    ) {                                                                        \
        return (ArrayList_##T) {                                               \
            .T_TypeInfo = T_TypeInfo,                                          \
            .buf = Kast_allocate_array(T_TypeInfo, capacity),                  \
            .capacity = capacity,                                              \
            .length = 0,                                                       \
        };                                                                     \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_reserve(ArrayList_##T* list, size_t capacity) {       \
        if (list->capacity < capacity) {                                       \
            size_t old_capacity = list->capacity;                              \
            list->capacity = (list->capacity == 0) ? 4 : (list->capacity * 2); \
            if (list->capacity < capacity) {                                   \
                list->capacity = capacity;                                     \
            }                                                                  \
            list->buf = Kast_reallocate_array(                                 \
                list->T_TypeInfo,                                              \
                list->buf,                                                     \
                old_capacity,                                                  \
                list->capacity                                                 \
            );                                                                 \
        }                                                                      \
    }                                                                          \
                                                                               \
    void ArrayList_##T##_push_back(ArrayList_##T* list, T x) {                 \
        ArrayList_##T##_reserve(list, list->length + 1);                       \
        list->buf[list->length++] = x;                                         \
    }                                                                          \
                                                                               \
    T ArrayList_##T##_pop_back(ArrayList_##T* list) {                          \
        return list->buf[--list->length];                                      \
    }

#endif
