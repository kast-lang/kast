#ifndef __KAST_ARRAY_LIST_H__
#define __KAST_ARRAY_LIST_H__

#include "./__common__.h"
#include "./claim.h"
#include "./fmt.h"

#define define_ArrayList(name, T)                                              \
    typedef struct {                                                           \
        TypeInfo* T_TypeInfo;                                                  \
        T* buf;                                                                \
        size_t capacity;                                                       \
        size_t length;                                                         \
        Kast_Claimed claimed;                                                  \
    } name;                                                                    \
    name name##_new(TypeInfo* T_TypeInfo);                                     \
    void name##_dbg_write(name* list, Kast_Formatter* fmt);                    \
    name name##_claim(name* list);                                             \
    void name##_drop(name list);                                               \
    name name##_with_capacity(TypeInfo* T_TypeInfo, size_t capacity);          \
    void name##_reserve(name* list, size_t capacity);                          \
    void name##_push_back(name* list, T x);                                    \
    T name##_pop_back(name* list)

#define impl_ArrayList(name, T)                                                \
    name name##_new(TypeInfo* T_TypeInfo) {                                    \
        return (name) {                                                        \
            .T_TypeInfo = T_TypeInfo,                                          \
            .buf = NULL,                                                       \
            .capacity = 0,                                                     \
            .length = 0,                                                       \
            .claimed = Kast_Claimed_init("ArrayList"),                         \
        };                                                                     \
    }                                                                          \
                                                                               \
    void name##_dbg_write(name* list, Kast_Formatter* fmt) {                   \
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
    name name##_claim(name* list) {                                            \
        name moved = *list;                                                    \
        Kast_mark_as_claimed(&list->claimed);                                  \
        return moved;                                                          \
    }                                                                          \
                                                                               \
    void name##_drop(name list) {                                              \
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
    name name##_with_capacity(TypeInfo* T_TypeInfo, size_t capacity) {         \
        return (name) {                                                        \
            .T_TypeInfo = T_TypeInfo,                                          \
            .buf = Kast_allocate_array(T_TypeInfo, capacity),                  \
            .capacity = capacity,                                              \
            .length = 0,                                                       \
            .claimed = Kast_Claimed_init("ArrayList"),                         \
        };                                                                     \
    }                                                                          \
                                                                               \
    void name##_reserve(name* list, size_t capacity) {                         \
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
    void name##_push_back(name* list, T x) {                                   \
        name##_reserve(list, list->length + 1);                                \
        list->buf[list->length++] = x;                                         \
    }                                                                          \
                                                                               \
    T name##_pop_back(name* list) {                                            \
        return list->buf[--list->length];                                      \
    }

// Required by Kast_read_file_bytes
define_ArrayList(ArrayList_Byte, Byte);

#endif
