#ifndef __KAST_SYS_H__

#include "./__common__.h"
#include "./string.h"

void Kast_chdir(StringView path);
Int32 Kast_exec(StringView cmd);
String Kast_getenv(StringView name);

#endif
