#ifndef __KAST_CLOSURE_H__
#define __KAST_CLOSURE_H__

#include "./__common__.h"
#include "./ref_counted.h"
#include "./unwind.h"

#define define_closure_type_C(name, Ret, ...)                                  \
    typedef struct {                                                           \
        void* captured;                                                        \
        RefCounted_Unit refcount;                                              \
        TypeInfo* captured_TypeInfo;                                           \
        Ret (*f)(void*, ##__VA_ARGS__);                                        \
    } name;                                                                    \
    name name##_claim(name* place);                                            \
    void name##_drop(name closure)

#define impl_closure_type_C(name, Ret, ...)                                    \
    name name##_claim(name* place) {                                           \
        if (place->captured == NULL) {                                         \
            return *place;                                                     \
        }                                                                      \
        return (name) {                                                        \
            .refcount = RefCounted_Unit_claim(&place->refcount),               \
            .captured = place->captured,                                       \
            .captured_TypeInfo = place->captured_TypeInfo,                     \
            .f = place->f,                                                     \
        };                                                                     \
    }                                                                          \
                                                                               \
    void name##_drop(name closure) {                                           \
        if (closure.captured == NULL) {                                        \
            return;                                                            \
        }                                                                      \
        if (RefCounted_Unit_drop(closure.refcount) == 0) {                     \
            if (closure.captured_TypeInfo->drop != NULL) {                     \
                closure.captured_TypeInfo->drop(closure.captured);             \
            }                                                                  \
            Kast_free(closure.captured);                                       \
        }                                                                      \
    }

#define define_closure_type(name, Ret, ...)                                    \
    define_closure_type_C(name, Ret, Context*, ##__VA_ARGS__)
#define impl_closure_type(name, Ret, ...)                                      \
    impl_closure_type_C(name, Ret, Context*, ##__VA_ARGS__)

#define call_closure(perform_unwind, closure, ...)                             \
    (closure).f((closure).captured, ctx, ##__VA_ARGS__);                       \
    if (are_we_unwinding()) {                                                  \
        perform_unwind;                                                        \
    }

define_closure_type(fn_Int32_Char_Unit, void, Int32, Char);
define_closure_type(fn_Char_Unit, void, Char);

#endif
