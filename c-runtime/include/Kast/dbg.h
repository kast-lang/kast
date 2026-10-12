#ifndef __KAST_DBG_H__
#define __KAST_DBG_H__

#include "./fmt.h"
#include "./type_info.h"

void Kast_dbg_write(void* value, TypeInfo* T, Kast_Formatter* fmt);
void Kast_dbg_print(void* value, TypeInfo* T);

#endif
