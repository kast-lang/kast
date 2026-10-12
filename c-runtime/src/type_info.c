#include <Kast/type_info.h>

void Byte_dbg_write(Byte value, Kast_Formatter* fmt);
void Byte_dbg_write_type_erased(void* value, Kast_Formatter* fmt);
TypeInfo Byte_TypeInfo = TypeInfo_simple(primitive, Byte);

void Kast_init_type_infos(void) {}

void TypeInfo_drop(TypeInfo* T, void* value) {
    if (T->drop != NULL) {
        T->drop(value);
    }
}
