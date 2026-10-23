#ifndef __KAST_PTR_SET_H__
#define __KAST_PTR_SET_H__

#include "./__common__.h"

typedef struct PtrMapData* PtrMap;

PtrMap PtrMap_new(void);
void PtrMap_add(PtrMap* map, void* key, void* value);
bool PtrMap_contains(PtrMap* map, void* key);
void* PtrMap_get(PtrMap* map, void* key);
void* PtrMap_remove(PtrMap* map, void* key);
void PtrMap_drop(PtrMap map);
void PtrMap_iter(PtrMap* map, void (*f)(void* key, void* value));

#endif
