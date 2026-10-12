#include <Kast/mem.h>
#include <Kast/type_info.h>

void* Kast_allocate_array(TypeInfo* T, size_t length) {
    size_t memory_size = T->stride * length;
    if (memory_size == 0) {
        return NULL;
    }
    void* result = malloc(memory_size);
    if (!result) {
        exit_errno(
            "Kast_allocate_array failed allocating %zu elements of %s (stride %zu), total memory = %zu",
            length,
            T->name,
            T->stride,
            memory_size
        );
    }
    return result;
}

void* Kast_allocate(TypeInfo* T) {
    return Kast_allocate_array(T, 1);
}

void* Kast_reallocate_array(
    TypeInfo* T,
    void* a,
    size_t old_length,
    size_t new_length
) {
    size_t new_memory_size = T->stride * new_length;
#ifdef USE_GC
    void* result = Kast_allocate_array(T, new_length);
    if (a != NULL) {
        memcpy(result, a, new_memory_size);
    }
#else
    if (new_memory_size == 0) {
        free(a);
        return NULL;
    } else {
        void* result = realloc(a, new_memory_size);
        if (!result) {
            exit_errno(
                "Kast_reallocate_array failed allocating %zu elements of %s (stride %zu), old_length = %zu, total memory = %zu",
                new_length,
                T->name,
                T->stride,
                old_length,
                new_memory_size
            );
        }
        return result;
    }
#endif
}

void Kast_free(const void* memory) {
    free((void*)memory);
}

void* Kast_allocate_raw(size_t length) {
    return Kast_allocate_array(&Byte_TypeInfo, length);
}
