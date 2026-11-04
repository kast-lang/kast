#include <Kast/mem.h>
#include <Kast/sort.h>

#define offset_ptr(a, i) (void*)((char*)(a) + (ptrdiff_t)(size) * (i))

size_t gcd(size_t a, size_t b) {
    if (a == 0) {
        return b;
    }
    if (b == 0) {
        return a;
    }
    if (a > b) {
        return gcd(b, a);
    }
    return gcd(b - a, a);
}

void memrotate(void* a, size_t size, size_t shift) {
    if (shift == size) {
        return;
    }
    char* a_bytes = a;
    size_t cycles = gcd(size, shift);
    for (size_t s = 0; s < cycles; s++) {
        size_t i = s;
        do {
            size_t next = (i + shift) % size;
            assert(next != i);
            char t = a_bytes[s];
            a_bytes[s] = a_bytes[next];
            a_bytes[next] = t;
            i = next;
        } while (i != s);
    }
}

void sort_impl(
    void* buf,
    void* a,
    size_t length,
    size_t size,
    int (*compare)(void* a, void* b, void* user),
    void* user
) {
    if (length <= 1) {
        return;
    }
    size_t mid = length / 2;
    sort_impl(buf, a, mid, size, compare, user);
    sort_impl(buf, offset_ptr(a, mid), length - mid, size, compare, user);
    void* merged_ptr = buf;
    size_t i = 0;
    size_t j = mid;
    while (i < mid && j < length) {
        if (compare(offset_ptr(a, i), offset_ptr(a, j), user) <= 0) {
            memcpy(merged_ptr, offset_ptr(a, i), size);
            i++;
        } else {
            memcpy(merged_ptr, offset_ptr(a, j), size);
            j++;
        }
        merged_ptr = offset_ptr(merged_ptr, 1);
    }
    while (i < mid) {
        memcpy(merged_ptr, offset_ptr(a, i), size);
        merged_ptr = offset_ptr(merged_ptr, 1);
        i++;
    }
    while (j < length) {
        memcpy(merged_ptr, offset_ptr(a, j), size);
        merged_ptr = offset_ptr(merged_ptr, 1);
        j++;
    }
    memcpy(a, buf, length * size);
}

void Kast_sort(
    void* a,
    size_t length,
    size_t size,
    int (*compare)(void* a, void* b, void* user),
    void* user
) {
    if (length <= 1) {
        return;
    }
    void* buf = Kast_allocate_raw(length * size);
    sort_impl(buf, a, length, size, compare, user);
    Kast_free(buf);
}
