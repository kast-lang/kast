#include <Kast/type_info.h>

TypeInfo Byte_TypeInfo = TypeInfo_simple(primitive, Byte);

void Kast_init_type_infos(void) {}

void TypeInfo_drop(TypeInfo* T, void* value) {
    if (T->drop != NULL) {
        T->drop(value);
    }
}
