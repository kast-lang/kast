#include <Kast/mem.h>
#include <Kast/ptr_map.h>

typedef struct KV {
    void* key;
    void* value;
    struct KV* next;
} KV;

typedef struct {
    KV* pairs;
} Bucket;

Bucket Bucket_new(void) {
    return (Bucket) {
        .pairs = NULL,
    };
}

void Bucket_drop(Bucket self) {
    KV* kv = self.pairs;
    while (kv != NULL) {
        KV* next = kv->next;
        Kast_free(kv);
        kv = next;
    }
}

void* Bucket_remove(Bucket* self, void* key) {
    for (KV *kv = self->pairs, *prev = NULL; kv != NULL;
         prev = kv, kv = kv->next) {
        if (kv->key == key) {
            void* value = kv->value;
            if (prev == NULL) {
                self->pairs = kv->next;
            } else {
                prev->next = kv->next;
            }
            Kast_free(kv);
            return value;
        }
    }
    return NULL;
}

KV* Bucket_find(Bucket* self, void* key) {
    for (KV* kv = self->pairs; kv != NULL; kv = kv->next) {
        if (kv->key == key) {
            return kv;
        }
    }
    return NULL;
}

void Bucket_add(Bucket* self, void* key, void* value) {
    KV* existing = Bucket_find(self, key);
    if (existing == NULL) {
        KV* new_kv = Kast_allocate_raw(sizeof(KV));
        new_kv->key = key;
        new_kv->value = value;
        new_kv->next = self->pairs;
        self->pairs = new_kv;
    } else {
        existing->value = value;
    }
}

typedef struct PtrMapData {
    Bucket* buckets;
    size_t bucket_count;
    size_t size;
    size_t inc_buckets_threshold;
} PtrMapData;

PtrMap PtrMap_new(void) {
    PtrMap map;
    map = Kast_allocate_raw(sizeof(*map));
    *map = (PtrMapData) {
        .buckets = NULL,
        .bucket_count = 0,
        .size = 0,
        .inc_buckets_threshold = 0,
    };
    return map;
}

void drop_data(PtrMapData map) {
    for (size_t i = 0; i < map.bucket_count; i++) {
        Bucket_drop(map.buckets[i]);
    }
    Kast_free(map.buckets);
}

Bucket* get_bucket(PtrMapData* map, void* key) {
    if (map->bucket_count == 0) {
        exit_with_error("Trying to get bucket but no buckets");
    }
    return &map->buckets[((size_t)key) % map->bucket_count];
}

KV* get_impl(PtrMapData* map, void* key) {
    if (map->size == 0) {
        return NULL;
    }
    Bucket* bucket = get_bucket(map, key);
    return Bucket_find(bucket, key);
}

void* remove_impl(PtrMapData* map, void* key) {
    if (map->size == 0) {
        return NULL;
    }
    Bucket* bucket = get_bucket(map, key);
    return Bucket_remove(bucket, key);
}

void add_impl(PtrMapData* map, void* key, void* value) {
    if (get_impl(map, key) == NULL) {
        map->size++;
    }
    if (map->size > map->inc_buckets_threshold) {
        PtrMapData new_map = *map;
        size_t avg_bucket_size = 2;
        new_map.bucket_count =
            (map->size + avg_bucket_size - 1) / avg_bucket_size;
        new_map.buckets =
            Kast_allocate_raw(sizeof(Bucket) * new_map.bucket_count);
        for (size_t i = 0; i < new_map.bucket_count; i++) {
            new_map.buckets[i] = Bucket_new();
        }
        for (size_t i = 0; i < map->bucket_count; i++) {
            Bucket* bucket = &map->buckets[i];
            for (KV* kv = bucket->pairs; kv != NULL; kv = kv->next) {
                add_impl(&new_map, kv->key, kv->value);
            }
        }
        drop_data(*map);
        *map = new_map;
        map->inc_buckets_threshold = map->size * 2;
    }
    Bucket* bucket = get_bucket(map, key);
    Bucket_add(bucket, key, value);
}

void PtrMap_add(PtrMap* map, void* key, void* value) {
    add_impl(*map, key, value);
}

bool PtrMap_contains(PtrMap* map, void* key) {
    return get_impl(*map, key) != NULL;
}

void* PtrMap_get(PtrMap* map, void* key) {
    KV* kv = get_impl(*map, key);
    if (kv == NULL) {
        return NULL;
    } else {
        return kv->value;
    }
}

void* PtrMap_remove(PtrMap* map, void* key) {
    return remove_impl(*map, key);
}

void PtrMap_drop(PtrMap map) {
    drop_data(*map);
    Kast_free(map);
}

void iter_impl(PtrMapData* map, void (*f)(void* key, void* value)) {
    for (size_t i = 0; i < map->bucket_count; i++) {
        Bucket* bucket = &map->buckets[i];
        for (KV* kv = bucket->pairs; kv != NULL; kv = kv->next) {
            f(kv->key, kv->value);
        }
    }
}

void PtrMap_iter(PtrMap* map, void (*f)(void* key, void* value)) {
    iter_impl(*map, f);
}
