#ifdef KAST_ALLOCATION_STATS
#include <Kast/allocation_stats.h>
#include <Kast/type_info.h>

void Kast_type_allocation_stats_dump(Kast_type_allocation_stats* stats) {
    fprintf(
        stderr,
        "allocations=%zu total_memory=%zu",
        stats->allocations,
        stats->total_memory
    );
}

TypeInfo* TRACKED_TYPES_HEAD = NULL;

void Kast_ensure_type_is_tracked(TypeInfo* T) {
    if (T->allocation_stats.tracked) {
        return;
    }
    T->allocation_stats.tracked = true;
    T->allocation_stats.next_tracked = TRACKED_TYPES_HEAD;
    TRACKED_TYPES_HEAD = T;
}

int TypeInfo_compare(const void* void_a, const void* void_b) {
    TypeInfo* const* a = void_a;
    TypeInfo* const* b = void_b;
    return (int)(*a)->allocation_stats.allocations
        - (int)(*b)->allocation_stats.allocations;
}

struct {
    Kast_type_allocation_stats raw;
} Kast_allocation_stats;

void Kast_dump_allocation_stats(void) {
    fprintf(stderr, "RAW: ");
    Kast_type_allocation_stats_dump(&Kast_allocation_stats.raw);
    fprintf(stderr, "\n");
    size_t types_len = 0;
    for (TypeInfo* T = TRACKED_TYPES_HEAD; T != NULL;
         T = T->allocation_stats.next_tracked) {
        types_len++;
    }
    TypeInfo** types = malloc(sizeof(TypeInfo*) * types_len);
    size_t i = 0;
    for (TypeInfo* T = TRACKED_TYPES_HEAD; T != NULL;
         T = T->allocation_stats.next_tracked) {
        types[i++] = T;
    }
    if (i != types_len) {
        exit_with_error("???");
    }
    qsort(types, types_len, sizeof(TypeInfo*), TypeInfo_compare);
    for (i = 0; i < types_len; i++) {
        TypeInfo* T = types[i];
        fprintf(stderr, "%s: ", T->name);
        Kast_type_allocation_stats_dump(&T->allocation_stats);
        fprintf(stderr, "\n");
    }
    free(types);
}

#endif
